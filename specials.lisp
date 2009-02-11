;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/specials.lisp,v 1.33 2008/04/08 14:39:18 edi Exp $

;;; Copyright (c) 2004-2009, Dr. Edmund Weitz. All rights reserved.

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

(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defmacro defvar-unbound (name &optional (doc-string ""))
    "Convenience macro to declare unbound special variables with a
documentation string."
    `(progn
      (defvar ,name)
      (setf (documentation ',name 'variable) ,doc-string)))

  (defvar *http-reason-phrase-map* (make-hash-table)
    "Used to map numerical return codes to reason phrases.")
  
  (defmacro def-http-return-code (name value reason-phrase)
    "Shortcut to define constants for return codes.  NAME is a
Lisp symbol, VALUE is the numerical value of the return code, and
REASON-PHRASE is the phrase \(a string) to be shown in the
server's status line."
    `(eval-when (:compile-toplevel :execute :load-toplevel)
       (defconstant ,name ,value ,(format nil "HTTP return code \(~A) for '~A'."
                                          value reason-phrase))
       (setf (gethash ,value *http-reason-phrase-map*) ,reason-phrase))))

(defconstant +crlf+
  (make-array 2 :element-type '(unsigned-byte 8)
              :initial-contents (mapcar 'char-code '(#\Return #\Linefeed)))
  "A 2-element array consisting of the character codes for a CRLF
sequence.")

(def-http-return-code +http-continue+ 100 "Continue")
(def-http-return-code +http-switching-protocols+ 101 "Switching Protocols")
(def-http-return-code +http-ok+ 200 "OK")
(def-http-return-code +http-created+ 201 "Created")
(def-http-return-code +http-accepted+ 202 "Accepted")
(def-http-return-code +http-non-authoritative-information+ 203 "Non-Authoritative Information")
(def-http-return-code +http-no-content+ 204 "No Content")
(def-http-return-code +http-reset-content+ 205 "Reset Content")
(def-http-return-code +http-partial-content+ 206 "Partial Content")
(def-http-return-code +http-multi-status+ 207 "Multi-Status")
(def-http-return-code +http-multiple-choices+ 300 "Multiple Choices")
(def-http-return-code +http-moved-permanently+ 301 "Moved Permanently")
(def-http-return-code +http-moved-temporarily+ 302 "Moved Temporarily")
(def-http-return-code +http-see-other+ 303 "See Other")
(def-http-return-code +http-not-modified+ 304 "Not Modified")
(def-http-return-code +http-use-proxy+ 305 "Use Proxy")
(def-http-return-code +http-temporary-redirect+ 307 "Temporary Redirect")
(def-http-return-code +http-bad-request+ 400 "Bad Request")
(def-http-return-code +http-authorization-required+ 401 "Authorization Required")
(def-http-return-code +http-payment-required+ 402  "Payment Required")
(def-http-return-code +http-forbidden+ 403 "Forbidden")
(def-http-return-code +http-not-found+ 404 "Not Found")
(def-http-return-code +http-method-not-allowed+ 405 "Method Not Allowed")
(def-http-return-code +http-not-acceptable+ 406 "Not Acceptable")
(def-http-return-code +http-proxy-authentication-required+ 407 "Proxy Authentication Required")
(def-http-return-code +http-request-time-out+ 408 "Request Time-out")
(def-http-return-code +http-conflict+ 409 "Conflict")
(def-http-return-code +http-gone+ 410 "Gone")
(def-http-return-code +http-length-required+ 411 "Length Required")
(def-http-return-code +http-precondition-failed+ 412 "Precondition Failed")
(def-http-return-code +http-request-entity-too-large+ 413 "Request Entity Too Large")
(def-http-return-code +http-request-uri-too-large+ 414 "Request-URI Too Large")
(def-http-return-code +http-unsupported-media-type+ 415 "Unsupported Media Type")
(def-http-return-code +http-requested-range-not-satisfiable+ 416 "Requested range not satisfiable")
(def-http-return-code +http-expectation-failed+ 417 "Expectation Failed")
(def-http-return-code +http-failed-dependency+ 424 "Failed Dependency")
(def-http-return-code +http-internal-server-error+ 500 "Internal Server Error")
(def-http-return-code +http-not-implemented+ 501 "Not Implemented")
(def-http-return-code +http-bad-gateway+ 502 "Bad Gateway")
(def-http-return-code +http-service-unavailable+ 503 "Service Unavailable")
(def-http-return-code +http-gateway-time-out+ 504 "Gateway Time-out")
(def-http-return-code +http-version-not-supported+ 505 "Version not supported")

(defvar *approved-return-codes* '(#.+http-ok+ #.+http-no-content+
                                              #.+http-multi-status+
                                              #.+http-not-modified+)
  "A list of return codes the server should not treat as an error -
see *HANDLE-HTTP-ERRORS-P*.")

(defconstant +day-names+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "The three-character names of the seven days of the week - needed
for cookie date format.")

(defconstant +month-names+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "The three-character names of the twelve months - needed for cookie
date format.")

(defvar *session-cookie-name* "hunchentoot-session"
  "The name of the cookie \(or the GET parameter) which is used to
store the session on the client side.")

(defvar *rewrite-for-session-urls* t
  "Whether HTML pages should possibly be rewritten for cookie-less
session-management.")

(defvar *content-types-for-url-rewrite*
  '("text/html" "application/xhtml+xml")
  "The content types for which url-rewriting is OK. See
*REWRITE-FOR-SESSION-URLS*.")

(defparameter *the-random-state* (make-random-state t)
  "A fresh random state.")

(defvar-unbound *session-secret*
  "A random ASCII string that's used to encode the public session
data.  This variable is initially unbound and will be set \(using
RESET-SESSION-SECRET) by the first acceptor which is started.  You can
prevent this from happening if you set the value yourself before
starting acceptors.")

(defvar-unbound *hunchentoot-stream*
  "The stream representing the socket Hunchentoot is listening on.")

(defvar *close-hunchentoot-stream* nil
  "Will be set to T if the Hunchentoot socket stream has to be
closed at the end of the request.")

(defvar *headers-sent* nil
  "Used internally to check whether the reply headers have
already been sent for this request.")

(defvar *file-upload-hook* nil
  "If this is not NIL, it should be a unary function which will
be called with a pathname for each file which is uploaded to
Hunchentoot.  The pathname denotes the temporary file to which
the uploaded file is written.  The hook is called directly before
the file is created.")

(defvar *session-data* nil
  "All sessions of all users currently using Hunchentoot.  An
alist where the car is the session's ID and the cdr is the
SESSION object itself.")

(defvar *session-max-time* #.(* 30 60)
  "The default time \(in seconds) after which a session times out.")

(defvar *session-gc-frequency* 50
  "A session GC \(see function SESSION-GC) will happen every
*SESSION-GC-FREQUENCY* requests \(counting only requests which
use a session) if this variable is not NIL.")

(defvar *use-user-agent-for-sessions* t
  "Whether the 'User-Agent' header should be encoded into the session
string.  If this value is true, a session will cease to be accessible
if the client sends a different 'User-Agent' header.")

(defvar *use-remote-addr-for-sessions* nil
  "Whether the client's remote IP \(as returned by REAL-REMOTE-ADDR)
should be encoded into the session string.  If this value is true, a
session will cease to be accessible if the client's remote IP changes.

This might for example be an issue if the client uses a proxy server
which doesn't send correct 'X_FORWARDED_FOR' headers.")

(defvar *default-content-type* "text/html; charset=iso-8859-1"
  "The default content-type header which is returned to the client.")

(defvar *methods-for-post-parameters* '(:post)
  "A list of the request method types \(as keywords) for which
Hunchentoot will try to compute POST-PARAMETERS.")

(defvar *header-stream* nil
  "If this variable is not NIL, it should be bound to a stream to
which incoming and outgoing headers will be written for debugging
purposes.")

(defvar *show-lisp-errors-p* nil
  "Whether Lisp errors should be shown in HTML output.")

(defvar *log-lisp-errors-p* t
  "Whether Lisp errors should be logged.")

(defvar *log-lisp-warnings-p* t
  "Whether Lisp warnings should be logged.")

(defvar *lisp-errors-log-level* :error
  "Log level for Lisp errors.")

(defvar *lisp-warnings-log-level* :warning
  "Log level for Lisp warnings.")

(defvar *log-pathname* #P"hunchentoot-error.log"
  "The error log file to use.")

(defvar *access-log-pathname* #P"hunchentoot-access.log"
  "The access log file to use.")

(defvar-unbound *session*
  "The current SESSION object.")

(defvar-unbound *request*
  "The current REQUEST object.")

(defvar-unbound *reply*
  "The current REPLY object.")

(defconstant +implementation-link+
  #+:cmu "http://www.cons.org/cmucl/"
  #+:sbcl "http://www.sbcl.org/"
  #+:allegro "http://www.franz.com/products/allegrocl/"
  #+:lispworks "http://www.lispworks.com/"
  #+:openmcl "http://openmcl.clozure.com/"
  "A link to the website of the underlying Lisp implementation.")

(defvar *dispatch-table* (list 'default-dispatcher)
  "A global list of dispatch functions.")

(defvar *default-handler* 'default-handler
  "The name of the function which is always returned by
DEFAULT-DISPATCHER.")

(defvar *easy-handler-alist* nil
  "An alist of \(URI server-names function) lists defined by
DEFINE-EASY-HANDLER.")

(defvar *http-error-handler* nil
  "Contains NIL \(the default) or a function of one argument which is
called if the content handler has set a return code which is not in
*APPROVED-RETURN-CODES* and *HANDLE-HTTP-ERRORS* is true.")

(defvar *handle-http-errors-p* t
  "A generalized boolean that determines whether return codes which
are not in *APPROVED-HEADERS* are treated specially.  When its value
is true \(the default), either a default body for the return code or
the result of calling *HTTP-ERROR-HANDLER* is used.  When the value is
NIL, no special action is taken and you are expected to supply your
own response body to describe the error.")

(defvar *session-removal-hook* (constantly nil)
  "A function of one argument \(a session object) which is called
whenever a session is garbage-collected.")

(defvar *tmp-directory*
  #+(or :win32 :mswindows) "c:\\hunchentoot-temp\\"
  #-(or :win32 :mswindows) "/tmp/hunchentoot/"
  "Directory for temporary files created by MAKE-TMP-FILE-NAME.")

(defvar *tmp-files* nil
  "A list of temporary files created while a request was handled.")

(defconstant +latin-1+
  (make-external-format :latin1 :eol-style :lf)
  "A FLEXI-STREAMS external format used for `faithful' input and
output of binary data.")

(defconstant +utf-8+
  (make-external-format :utf8 :eol-style :lf)
  "A FLEXI-STREAMS external format used internally for logging and to
encode cookie values.")

(defvar *hunchentoot-default-external-format* +latin-1+
  "The external format used to compute the REQUEST object.")

(defconstant +buffer-length+ 8192
  "Length of buffers used for internal purposes.")

(defvar-unbound *acceptor*
  "During the execution of dispatchers and handlers this variable
is bound to the SERVER object which processes the request.")

(defvar *default-connection-timeout* 20
  "The default connection timeout used when a Hunchentoot server is
reading from and writing to a socket stream.")

(defvar-unbound *local-host*
  "Bound to a string denoting the address at which the current
request arrived.")

(defvar-unbound *remote-host*
  "Bound to a string denoting the address the current request
originated from.")

(defvar-unbound *remote-port*
  "Bound to an integer denoting the port the current request
originated from.")

(define-symbol-macro *supports-threads-p*
  #+:lispworks t
  #-:lispworks bt:*supports-threads-p*)

(defconstant +new-connection-wait-time+ 2
  "Time in seconds to wait for a new connection to arrive before
performing a cleanup run.")

(pushnew :hunchentoot *features*)

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>

(defvar *hyperdoc-base-uri* "http://weitz.de/hunchentoot/")

(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :hunchentoot
             collect (cons symbol (concatenate 'string "#" (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol exported-symbols-alist :test #'eq))))
