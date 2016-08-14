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
      (setf (documentation ',name 'variable) ,doc-string)
      ',name))

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
(def-http-return-code +http-precondition-required+ 428 "Precondition Required")
(def-http-return-code +http-too-many-requests+ 429 "Too Many Requests")
(def-http-return-code +http-request-header-fields-too-large+ 431 "Request Header Fields Too Large")
(def-http-return-code +http-internal-server-error+ 500 "Internal Server Error")
(def-http-return-code +http-not-implemented+ 501 "Not Implemented")
(def-http-return-code +http-bad-gateway+ 502 "Bad Gateway")
(def-http-return-code +http-service-unavailable+ 503 "Service Unavailable")
(def-http-return-code +http-gateway-time-out+ 504 "Gateway Time-out")
(def-http-return-code +http-version-not-supported+ 505 "Version not supported")
(def-http-return-code +http-network-authentication-required+ 511 "Network Authentication Required")

(defconstant +day-names+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "The three-character names of the seven days of the week - needed
for cookie date format.")

(defconstant +month-names+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "The three-character names of the twelve months - needed for cookie
date format.")

(defvar *rewrite-for-session-urls* t
  "Whether HTML pages should possibly be rewritten for cookie-less
session-management.")

(defvar *content-types-for-url-rewrite*
  '("text/html" "application/xhtml+xml")
  "The content types for which url-rewriting is OK. See
*REWRITE-FOR-SESSION-URLS*.")

(defvar *the-random-state* (make-random-state t)
  "A fresh random state.")

(defvar-unbound *session-secret*
  "A random ASCII string that's used to encode the public session
data.  This variable is initially unbound and will be set \(using
RESET-SESSION-SECRET) the first time a session is created, if
necessary.  You can prevent this from happening if you set the value
yourself before starting acceptors.")

(defvar-unbound *hunchentoot-stream*
  "The stream representing the socket Hunchentoot is listening on.")

(defvar-unbound *finish-processing-socket*
  "Will be set to T if PROCESS-CONNECTION is to stop processing more
  requests on the current socket connection.")

(defvar-unbound *close-hunchentoot-stream*
  "This variable is set to NIL during the processing of a handler to
tell the acceptor not to close the connection after it is done.")

(defvar *headers-sent* nil
  "Used internally to check whether the reply headers have
already been sent for this request.")

(defvar *file-upload-hook* nil
  "If this is not NIL, it should be a unary function which will
be called with a pathname for each file which is uploaded to
Hunchentoot.  The pathname denotes the temporary file to which
the uploaded file is written.  The hook is called directly before
the file is created.")

(defvar *session-db* nil
  "The default \(global) session database.")

(defvar *session-max-time* #.(* 30 60)
  "The default time \(in seconds) after which a session times out.")

(defvar *session-gc-frequency* 50
  "A session GC \(see function SESSION-GC) will happen every
*SESSION-GC-FREQUENCY* requests \(counting only requests which create
a new session) if this variable is not NIL.  See SESSION-CREATED.")

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

(defvar *default-content-type* "text/html"
  "The default content-type header which is returned to the client.
If this is text content type, the character set used for encoding the
response will automatically be added to the content type in a
``charset'' attribute.")

(defvar *methods-for-post-parameters* '(:post)
  "A list of the request method types \(as keywords) for which
Hunchentoot will try to compute POST-PARAMETERS.")

(defvar *header-stream* nil
  "If this variable is not NIL, it should be bound to a stream to
which incoming and outgoing headers will be written for debugging
purposes.")

(defvar *show-lisp-errors-p* nil
  "Whether Lisp errors in request handlers should be shown in HTML output.")

(defvar *show-lisp-backtraces-p* t
  "Whether Lisp errors shown in HTML output should contain backtrace information.")

(defvar *log-lisp-errors-p* t
  "Whether Lisp errors in request handlers should be logged.")

(defvar *log-lisp-backtraces-p* t
  "Whether Lisp backtraces should be logged.  Only has an effect if
*LOG-LISP-ERRORS-P* is true as well.")

(defvar *log-lisp-warnings-p* t
  "Whether Lisp warnings in request handlers should be logged.")

(defvar *lisp-errors-log-level* :error
  "Log level for Lisp errors.  Should be one of :ERROR \(the default),
:WARNING, or :INFO.")

(defvar *lisp-warnings-log-level* :warning
  "Log level for Lisp warnings.  Should be one of :ERROR, :WARNING
\(the default), or :INFO.")

(defvar *message-log-lock* (make-lock "global-message-log-lock")
  "A global lock to prevent concurrent access to the log file used by
the ACCEPTOR-LOG-MESSAGE function.")

(defvar *access-log-lock* (make-lock "global-access-log-lock")
  "A global lock to prevent concurrent access to the log file used by
the ACCEPTOR-LOG-ACCESS function.")

(defvar *catch-errors-p* t
  "Whether Hunchentoot should catch and log errors \(or rather invoke
the debugger).")

(defvar-unbound *acceptor*
  "The current ACCEPTOR object while in the context of a request.")

(defvar-unbound *request*
  "The current REQUEST object while in the context of a request.")

(defvar-unbound *reply*
  "The current REPLY object while in the context of a request.")

(defvar-unbound *session*
  "The current session while in the context of a request, or NIL.")

(defconstant +implementation-link+
  #+:cmu "http://www.cons.org/cmucl/"
  #+:sbcl "http://www.sbcl.org/"
  #+:allegro "http://www.franz.com/products/allegrocl/"
  #+:lispworks "http://www.lispworks.com/"
  #+:openmcl "http://openmcl.clozure.com/"
  "A link to the website of the underlying Lisp implementation.")

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

(defvar *hunchentoot-default-external-format* +utf-8+
  "The external format used to compute the REQUEST object.")

(defconstant +buffer-length+ 8192
  "Length of buffers used for internal purposes.")

(defvar *default-connection-timeout* 20
  "The default connection timeout used when an acceptor is reading
from and writing to a socket stream.")

(eval-when (:compile-toplevel :load-toplevel :execute)
 (define-symbol-macro *supports-threads-p*
   #+:lispworks t
   #-:lispworks bt:*supports-threads-p*))

(defvar *global-session-db-lock*
  (load-time-value (and *supports-threads-p* (make-lock "global-session-db-lock")))
  "A global lock to prevent two threads from modifying *session-db* at
the same time \(or NIL for Lisps which don't have threads).")

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

(defparameter hunchentoot:*hunchentoot-version* #.(asdf:component-version (asdf:find-system :hunchentoot)))
