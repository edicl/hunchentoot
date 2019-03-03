;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :hunchentoot)

(defvar *dispatch-table* (list 'dispatch-easy-handlers)
  "A global list of dispatch functions.")

(defvar *easy-handler-alist* nil
  "An alist of \(URI acceptor-names function host) lists defined by
DEFINE-EASY-HANDLER.")

(defun compute-real-name (symbol)
  "Computes the `real' paramater name \(a string) from the Lisp
symbol SYMBOL.  Used in cases where no parameter name is
provided."
  ;; we just downcase the symbol's name
  (string-downcase symbol))

(defun convert-parameter (argument type)
  "Converts the string ARGUMENT to TYPE where TYPE is one of the
symbols STRING, CHARACTERS, INTEGER, KEYWORD, or BOOLEAN - or
otherwise a function designator for a function of one argument.
ARGUMENT can also be NIL in which case this function also returns
NIL unconditionally."
  (when (listp argument)
    ;; this if for the case that ARGUMENT is NIL or the result of a
    ;; file upload
    (return-from convert-parameter argument))
  (case type
    (string argument)
    (character (and (= (length argument) 1)
                    (char argument 0)))
    (integer (ignore-errors* (parse-integer argument :junk-allowed t)))
    (keyword (as-keyword argument :destructivep nil))
    (boolean t)
    (otherwise (funcall type argument))))

(defun compute-simple-parameter (parameter-name type parameter-reader)
  "Retrieves the parameter named PARAMETER-NAME using the reader
PARAMETER-READER and converts it to TYPE."
  (convert-parameter (funcall parameter-reader parameter-name) type))

(defun compute-list-parameter (parameter-name type parameters)
  "Retrieves all parameters from PARAMETERS which are named
PARAMETER-NAME, converts them to TYPE, and returns a list of
them."
  (loop for (name . value) in parameters
        when (string= name parameter-name)
        collect (convert-parameter value type)))

(defun compute-array-parameter (parameter-name type parameters)
  "Retrieves all parameters from PARAMETERS which are named like
\"PARAMETER-NAME[N]\" \(where N is a non-negative integer),
converts them to TYPE, and returns an array where the Nth element
is the corresponding value."
  ;; see <http://common-lisp.net/pipermail/tbnl-devel/2006-September/000660.html>
  #+:sbcl (declare (sb-ext:muffle-conditions warning))
  (let* ((index-value-list
          (loop for (full-name . value) in parameters
                for index = (register-groups-bind (name index-string)
                                ("^(.*)\\[(\\d+)\\]$" full-name)
                              (when (string= name parameter-name)
                                (parse-integer index-string)))
                when index
                collect (cons index (convert-parameter value type))))
         (array (make-array (1+ (reduce #'max index-value-list
                                        :key #'car
                                        :initial-value -1))
                            :initial-element nil)))
    (loop for (index . value) in index-value-list
          do (setf (aref array index) value))
    array))

(defun compute-hash-table-parameter (parameter-name type parameters key-type test-function)
  "Retrieves all parameters from PARAMETERS which are named like
\"PARAMETER-NAME{FOO}\" \(where FOO is any sequence of characters
not containing curly brackets), converts them to TYPE, and
returns a hash table with test function TEST-FUNCTION where the
corresponding value is associated with the key FOO \(converted to
KEY-TYPE)."
  (let ((hash-table (make-hash-table :test test-function)))
    (loop for (full-name . value) in parameters
          for key = (register-groups-bind (name key-string)
                        ("^(.*){([^{}]+)}$" full-name)
                      (when (string= name parameter-name)
                        (convert-parameter key-string key-type)))
          when key
          do (setf (gethash key hash-table)
                   (convert-parameter value type)))
    hash-table))

(defun compute-parameter (parameter-name parameter-type request-type)
  "Computes and returns the parameter\(s) called PARAMETER-NAME
and converts it/them according to the value of PARAMETER-TYPE.
REQUEST-TYPE is one of :GET, :POST, or :BOTH."
  (when (member parameter-type '(list array hash-table))
    (setq parameter-type (list parameter-type 'string)))
  (let ((parameter-reader (ecase request-type
                              (:get #'get-parameter)
                              (:post #'post-parameter)
                              (:both #'parameter)))
        (parameters (and (listp parameter-type)
                         (case request-type
                           (:get (get-parameters*))
                           (:post (post-parameters*))
                           (:both (append (get-parameters*) (post-parameters*)))))))
    (cond ((atom parameter-type)
           (compute-simple-parameter parameter-name parameter-type parameter-reader))
          ((and (null (cddr parameter-type))
                (eq (first parameter-type) 'list))
           (compute-list-parameter parameter-name (second parameter-type) parameters))
          ((and (null (cddr parameter-type))
                (eq (first parameter-type) 'array))
           (compute-array-parameter parameter-name (second parameter-type) parameters))
          ((and (null (cddddr parameter-type))
                (eq (first parameter-type) 'hash-table))
           (compute-hash-table-parameter parameter-name (second parameter-type) parameters
                                         (or (third parameter-type) 'string)
                                         (or (fourth parameter-type) 'equal)))
          (t (parameter-error "Don't know what to do with parameter type ~S." parameter-type)))))

(defun make-defun-parameter (description default-parameter-type default-request-type)
  "Creates a keyword parameter to be used by DEFINE-EASY-HANDLER.
DESCRIPTION is one of the elements of DEFINE-EASY-HANDLER's
LAMBDA-LIST and DEFAULT-PARAMETER-TYPE and DEFAULT-REQUEST-TYPE
are the global default values."
  (when (atom description)
    (setq description (list description)))
  (destructuring-bind (parameter-name &key (real-name (compute-real-name parameter-name))
                                           parameter-type init-form request-type)
      description
    `(,parameter-name (or (and (boundp '*request*)
                               (compute-parameter ,real-name
                                                  ,(or parameter-type default-parameter-type)
                                                  ,(or request-type default-request-type)))
                          ,init-form))))

(defmacro define-easy-handler (description lambda-list &body body)
  "Defines a handler with the body BODY and optionally registers
it with a URI so that it will be found by DISPATCH-EASY-HANDLERS.
DESCRIPTION is either a symbol NAME or a list matching the
destructuring lambda list

  (name &key uri acceptor-names host
        default-parameter-type default-request-type).

LAMBDA-LIST is a list the elements of which are either a symbol
VAR or a list matching the destructuring lambda list

  (var &key real-name parameter-type init-form request-type).

The resulting handler will be a Lisp function with the name NAME
and keyword parameters named by the VAR symbols.  Each VAR will
be bound to the value of the GET or POST parameter called
REAL-NAME \(a string) before BODY is executed.  If REAL-NAME is
not provided, it will be computed by downcasing the symbol name
of VAR.

If URI \(which is evaluated) is provided, then it must be a string or
a function designator for a function of one argument.  In this case,
the handler will be returned by DISPATCH-EASY-HANDLERS, if URI is a
string and the script name of a request is URI, or if URI designates a
function and applying this function to the current request object
returns a true value.
HOST, if given, is compared to the given host as well
(don't forget the port number).

ACCEPTOR-NAMES \(which is evaluated) can be a list of symbols which
means that the handler will be returned by DISPATCH-EASY-HANDLERS in
acceptors which have one of these names \(see ACCEPTOR-NAME).
ACCEPTOR-NAMES can also be the symbol T which means that the handler
will be returned by DISPATCH-EASY-HANDLERS in every acceptor.

Whether the GET or POST parameter \(or both) will be taken into
consideration, depends on REQUEST-TYPE which can
be :GET, :POST, :BOTH, or NIL.  In the last case, the value of
DEFAULT-REQUEST-TYPE \(the default of which is :BOTH) will be
used.

The value of VAR will usually be a string \(unless it resulted from a
file upload in which case it won't be converted at all), but if
PARAMETER-TYPE \(which is evaluated) is provided, the string will be
converted to another Lisp type by the following rules:

If the corresponding GET or POST parameter wasn't provided by the
client, VAR's value will be NIL.  If PARAMETER-TYPE is 'STRING, VAR's
value remains as is.  If PARAMETER-TYPE is 'INTEGER and the parameter
string consists solely of decimal digits, VAR's value will be the
corresponding integer, otherwise NIL.  If PARAMETER-TYPE is 'KEYWORD,
VAR's value will be the keyword obtained by interning the upcased
parameter string into the keyword package.  If PARAMETER-TYPE is
'CHARACTER and the parameter string is of length one, VAR's value will
be the single character of this string, otherwise NIL.  If
PARAMETER-TYPE is 'BOOLEAN, VAR's value will always be T \(unless it
is NIL by the first rule above, of course).  If PARAMETER-TYPE is any
other atom, it is supposed to be a function designator for a unary
function which will be called to convert the string to something else.

Those were the rules for `simple' types, but PARAMETER-TYPE can
also be a list starting with one of the symbols LIST, ARRAY, or
HASH-TABLE.  The second value of the list must always be a simple
parameter type as in the last paragraph - we'll call it the
`inner type' below.

In the case of 'LIST, all GET/POST parameters called REAL-NAME
will be collected, converted to the inner type, and assembled
into a list which will be the value of VAR.

In the case of 'ARRAY, all GET/POST parameters which have a name
like the result of

  (format nil \"~A[~A]\" real-name n)

where N is a non-negative integer, will be assembled into an
array where the Nth element will be set accordingly, after
conversion to the inner type.  The array, which will become the
value of VAR, will be big enough to hold all matching parameters,
but not bigger.  Array elements not set as described above will
be NIL.  Note that VAR will always be bound to an array, which
may be empty, so it will never be NIL, even if no appropriate
GET/POST parameters are found.

The full form of a 'HASH-TABLE parameter type is

  (hash-table inner-type key-type test-function),

but KEY-TYPE and TEST-FUNCTION can be left out in which case they
default to 'STRING and 'EQUAL, respectively.  For this parameter
type, all GET/POST parameters which have a name like the result
of

  (format nil \"~A{~A}\" real-name key)

\(where KEY is a string that doesn't contain curly brackets) will
become the values \(after conversion to INNER-TYPE) of a hash
table with test function TEST-FUNCTION where KEY \(after
conversion to KEY-TYPE) will be the corresponding key.  Note that
VAR will always be bound to a hash table, which may be empty, so
it will never be NIL, even if no appropriate GET/POST parameters
are found.

To make matters even more complicated, the three compound
parameter types also have an abbreviated form - just one of the
symbols LIST, ARRAY, or HASH-TABLE.  In this case, the inner type
will default to 'STRING.

If PARAMETER-TYPE is not provided or NIL, DEFAULT-PARAMETER-TYPE
\(the default of which is 'STRING) will be used instead.

If the result of the computations above would be that VAR would
be bound to NIL, then INIT-FORM \(if provided) will be evaluated
instead, and VAR will be bound to the result of this evaluation.

Handlers built with this macro are constructed in such a way that
the resulting Lisp function is useful even outside of
Hunchentoot.  Specifically, all the parameter computations above
will only happen if *REQUEST* is bound, i.e. if we're within a
Hunchentoot request.  Otherwise, VAR will always be bound to the
result of evaluating INIT-FORM unless a corresponding keyword
argument is provided."
  (when (atom description)
    (setq description (list description)))
  (destructuring-bind (name &key uri host (acceptor-names t)
                            (default-parameter-type ''string)
                            (default-request-type :both))
      description
    `(progn
       ,@(when uri
           (list
            (once-only (uri host acceptor-names)
              `(progn
                 (setq *easy-handler-alist*
                       (delete-if (lambda (list)
                                    (and (or (and (equal ,uri (first list))
                                                  ,(if host
                                                      `(string= ,host (fourth list))))
                                             (eq ',name (third list)))
                                         (or (eq ,acceptor-names t)
                                             (eq (second list) t)
                                             (intersection ,acceptor-names
                                                           (second list)))))
                                  *easy-handler-alist*))
                 (push (list ,uri ,acceptor-names ',name ,host) *easy-handler-alist*)))))
       (defun ,name (&key ,@(loop for part in lambda-list
                                  collect (make-defun-parameter part
                                                                default-parameter-type
                                                                default-request-type)))
         ,@body))))

;; help the LispWorks IDE to find these definitions
#+:lispworks
(dspec:define-form-parser define-easy-handler (description)
  `(,define-easy-handler ,(if (atom description) description (first description))))

#+:lispworks
(dspec:define-dspec-alias define-easy-handler (name)
  `(defun ,name))

(defun dispatch-easy-handlers (request)
  "This is a dispatcher which returns the appropriate handler
defined with DEFINE-EASY-HANDLER, if there is one."
  (loop for (uri acceptor-names easy-handler host) in *easy-handler-alist*
        when (and (or (eq acceptor-names t)
                      (find (acceptor-name *acceptor*) acceptor-names :test #'eq))
                  (cond ((stringp uri)
                         (and (or (null host)
                                  (string= (or (host request) "unknown")
                                           host))
                              ;; Support RE for matching host names as well (wildcards)?
                              (string= (script-name request) uri)))
                        (t (funcall uri request))))
        do (return easy-handler)))

(defclass easy-acceptor (acceptor)
  ()
  (:documentation "This is the acceptor of the ``easy'' Hunchentoot framework."))

(defmethod acceptor-dispatch-request ((acceptor easy-acceptor) request)
  "The easy request dispatcher which selects a request handler
based on a list of individual request dispatchers all of which can
either return a handler or neglect by returning NIL."
  (loop for dispatcher in *dispatch-table*
     for action = (funcall dispatcher request)
     when action return (funcall action)
     finally (call-next-method)))

#-:hunchentoot-no-ssl
(defclass easy-ssl-acceptor (easy-acceptor ssl-acceptor)
  ()
  (:documentation "This is an acceptor that mixes the ``easy''
  Hunchentoot with SSL connections."))
