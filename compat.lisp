;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz. All rights reserved.

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

(defmacro when-let ((var form) &body body)
  "Evaluates FORM and binds VAR to the result, then executes BODY
if VAR has a true value."
  `(let ((,var ,form))
     (when ,var ,@body)))

(defmacro with-unique-names ((&rest bindings) &body body)
  "Syntax: WITH-UNIQUE-NAMES ( { var | (var x) }* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  `(let ,(mapcar #'(lambda (binding)
                     (check-type binding (or cons symbol))
                     (if (consp binding)
                       (destructuring-bind (var x) binding
                         (check-type var symbol)
                         `(,var (gensym ,(etypecase x
                                          (symbol (symbol-name x))
                                          (character (string x))
                                          (string x)))))
                       `(,binding (gensym ,(symbol-name binding)))))
                 bindings)
         ,@body))

(defmacro with-rebinding (bindings &body body)
  "Syntax: WITH-REBINDING ( { var | (var prefix) }* ) form*

Evaluates a series of forms in the lexical environment that is
formed by adding the binding of each VAR to a fresh, uninterned
symbol, and the binding of that fresh, uninterned symbol to VAR's
original value, i.e., its value in the current lexical environment.

The uninterned symbol is created as if by a call to GENSYM with the
string denoted by PREFIX - or, if PREFIX is not supplied, the string
denoted by VAR - as argument.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3wv0fya0p.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  (loop for binding in bindings
        for var = (if (consp binding) (car binding) binding)
        for name = (gensym)
        collect `(,name ,var) into renames
        collect ``(,,var ,,name) into temps
        finally (return `(let ,renames
                          (with-unique-names ,bindings
                            `(let (,,@temps)
                              ,,@body))))))

(defun get-peer-address-and-port (socket)
  "Returns the peer address and port of the socket SOCKET as two
values.  The address is returned as a string in dotted IP address
notation."
  (multiple-value-bind (address port) (usocket:get-peer-name socket)
    (values (ecase (length address)
              (4 (usocket:vector-quad-to-dotted-quad address))
              #+(or) (16 (usocket:vector-to-ipv6-host address)))
            port)))

(defun get-local-address-and-port (socket)
  "Returns the local address and port of the socket SOCKET as two
values.  The address is returned as a string in dotted IP address
notation."
  (multiple-value-bind (address port) (usocket:get-local-name socket)
    (values (ecase (length address)
              (4 (usocket:vector-quad-to-dotted-quad address))
              #+(or) (16 (usocket:vector-to-ipv6-host address)))
            port)))

(defun make-socket-stream (socket acceptor)
  "Returns a stream for the socket SOCKET.  The ACCEPTOR argument is
ignored."
  (declare (ignore acceptor))
  (usocket:socket-stream socket))

(defun make-lock (name)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  (bt:make-lock name))

(defmacro with-lock-held ((lock) &body body)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  `(bt:with-lock-held (,lock) ,@body))

(defun make-condition-variable (&key name)
  (declare (ignore name))
  (bt:make-condition-variable))

(defun condition-variable-signal (condition-variable)
  (bt:condition-notify condition-variable))

(defun condition-variable-wait (condition-variable lock)
  (bt:condition-wait condition-variable lock))
