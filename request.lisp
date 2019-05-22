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

(defclass request ()
  ((acceptor :initarg :acceptor
             :documentation "The acceptor which created this request
object."
             :reader request-acceptor)
   (headers-in :initarg :headers-in
               :documentation "An alist of the incoming headers."
               :reader headers-in)
   (method :initarg :method
           :documentation "The request method as a keyword."
           :reader request-method)
   (uri :initarg :uri
        :documentation "The request URI as a string."
        :reader request-uri)
   (server-protocol :initarg :server-protocol
                    :documentation "The HTTP protocol as a keyword."
                    :reader server-protocol)
   (local-addr :initarg :local-addr
               :documentation "The IP address of the local system
that the client connected to."
               :reader local-addr)
   (local-port :initarg :local-port
               :documentation "The TCP port number of the local
system that the client connected to."
               :reader local-port)
   (remote-addr :initarg :remote-addr
                :documentation "The IP address of the client that
initiated this request."
                :reader remote-addr)
   (remote-port :initarg :remote-port
                :documentation "The TCP port number of the client
socket from which this request originated."
                :reader remote-port)
   (content-stream :initarg :content-stream
                   :reader content-stream
                   :documentation "A stream from which the request
body can be read if there is one.")
   (cookies-in :initform nil
               :documentation "An alist of the cookies sent by the client."
               :reader cookies-in)
   (get-parameters :initform nil
                   :documentation "An alist of the GET parameters sent
by the client."
                   :reader get-parameters)
   (post-parameters :initform nil
                    :documentation "An alist of the POST parameters
sent by the client."
                    :reader post-parameters)
   (script-name :initform nil
                :documentation "The URI requested by the client without
the query string."
                :reader script-name)
   (query-string :initform nil
                 :documentation "The query string of this request."
                 :reader query-string)
   (session :initform nil
            :accessor session
            :documentation "The session object associated with this
request.")
   (aux-data :initform nil
             :accessor aux-data
             :documentation "Used to keep a user-modifiable alist with
arbitrary data during the request.")
   (raw-post-data :initform nil
                  :documentation "The raw string sent as the body of a
POST request, populated only if not a multipart/form-data request."))
  (:documentation "Objects of this class hold all the information
about an incoming request.  They are created automatically by
acceptors and can be accessed by the corresponding handler.

You should not mess with the slots of these objects directly, but you
can subclass REQUEST in order to implement your own behaviour.  See
the REQUEST-CLASS slot of the ACCEPTOR class."))

(defgeneric process-request (request)
  (:documentation "This function is called by PROCESS-CONNECTION after
the incoming headers have been read.  It calls HANDLE-REQUEST to
select and call a handler and sends the output of this handler to the
client using START-OUTPUT.  Note that PROCESS-CONNECTION is called
once per connection and loops in case of a persistent connection while
PROCESS-REQUEST is called anew for each request.

Essentially, you can view process-request as a thin wrapper around
HANDLE-REQUEST.

The return value of this function is ignored."))

(defun convert-hack (string external-format)
  "The rfc2388 package is buggy in that it operates on a character
stream and thus only accepts encodings which are 8 bit transparent.
In order to support different encodings for parameter values
submitted, we post process whatever string values the rfc2388 package
has returned."
  (flex:octets-to-string (map '(vector (unsigned-byte 8) *) 'char-code string)
                         :external-format external-format))

(defun parse-rfc2388-form-data (stream content-type-header external-format)
  "Creates an alist of POST parameters from the stream STREAM which is
supposed to be of content type 'multipart/form-data'."
  (let* ((parsed-content-type-header (rfc2388:parse-header content-type-header :value))
	 (boundary (or (cdr (rfc2388:find-parameter
                             "BOUNDARY"
                             (rfc2388:header-parameters parsed-content-type-header)))
		       (return-from parse-rfc2388-form-data))))
    (loop for part in (rfc2388:parse-mime stream boundary)
          for headers = (rfc2388:mime-part-headers part)
          for content-disposition-header = (rfc2388:find-content-disposition-header headers)
          for name = (cdr (rfc2388:find-parameter
                           "NAME"
                           (rfc2388:header-parameters content-disposition-header)))
          when name
          collect (cons (convert-hack name external-format)
                        (let ((contents (rfc2388:mime-part-contents part)))
                          (if (pathnamep contents)
                            (list contents
                                  (convert-hack (rfc2388:get-file-name headers) external-format)
                                  (rfc2388:content-type part :as-string t))
                            (convert-hack contents external-format)))))))

(defun get-post-data (&key (request *request*) want-stream (already-read 0))
  "Reads the request body from the stream and stores the raw contents
\(as an array of octets) in the corresponding slot of the REQUEST
object.  Returns just the stream if WANT-STREAM is true.  If there's a
Content-Length header, it is assumed, that ALREADY-READ octets have
already been read."
  (let* ((headers-in (headers-in request))
         (content-length (when-let (content-length-header (cdr (assoc :content-length headers-in
                                                                      :test #'eq)))
                           (parse-integer content-length-header :junk-allowed t)))
         (content-stream (content-stream request)))
    (setf (slot-value request 'raw-post-data)
          (cond (want-stream
                 (let ((stream (make-flexi-stream content-stream :external-format +latin-1+)))
                   (when content-length
                     (setf (flexi-stream-bound stream) content-length))
                   stream))
                ((and content-length (> content-length already-read))
                 (decf content-length already-read)
                 (when (input-chunking-p)
                   ;; see RFC 2616, section 4.4
                   (log-message* :warning "Got Content-Length header although input chunking is on."))
                 (let ((content (make-array content-length :element-type 'octet)))
                   (read-sequence content content-stream)
                   content))
                ((input-chunking-p)
                 (loop with buffer = (make-array +buffer-length+ :element-type 'octet)
                       with content = (make-array 0 :element-type 'octet :adjustable t)
                       for index = 0 then (+ index pos)
                       for pos = (read-sequence buffer content-stream)
                       do (adjust-array content (+ index pos))
                          (replace content buffer :start1 index :end2 pos)
                       while (= pos +buffer-length+)
                       finally (return content)))))))

(defmethod initialize-instance :after ((request request) &rest init-args)
  "The only initarg for a REQUEST object is :HEADERS-IN.  All other
slot values are computed in this :AFTER method."
  (declare (ignore init-args))
  (with-slots (headers-in cookies-in get-parameters script-name query-string session)
      request
    (handler-case*
     (let* ((uri (request-uri request))
            (match-start (position #\? uri))
            (external-format (or (external-format-from-content-type (cdr (assoc* :content-type headers-in)))
                                 +utf-8+)))
       (cond
        (match-start
         (setq script-name (url-decode (subseq uri 0 match-start) external-format nil)
               query-string (subseq uri (1+ match-start))))
        (t (setq script-name (url-decode uri external-format nil))))
       ;; some clients (e.g. ASDF-INSTALL) send requests like
       ;; "GET http://server/foo.html HTTP/1.0"...
       (setq script-name (regex-replace "^https?://[^/]+" script-name ""))
       ;; compute GET parameters from query string and cookies from
       ;; the incoming 'Cookie' header
       (setq get-parameters
             (let ((*substitution-char* #\?))
               (form-url-encoded-list-to-alist (split "&" query-string) external-format))
             cookies-in
             (cookies-to-alist (split "\\s*[,;]\\s*" (cdr (assoc :cookie headers-in
                                                                 :test #'eq))))
             session (session-verify request)
             *session* session))
     (error (condition)
            (log-message* :error "Error when creating REQUEST object: ~A" condition)
            ;; we assume it's not our fault...
            (setf (return-code*) +http-bad-request+)))))

(defmethod process-request (request)
  "Standard implementation for processing a request."
  (catch 'request-processed ; used by HTTP HEAD handling to end request processing in a HEAD request (see START-OUTPUT)
    (let (*tmp-files*
          *headers-sent*
          (*request* request))
      (unwind-protect
           (with-mapped-conditions ()
             (labels
                 ((report-error-to-client (error &optional backtrace)
                    (when *log-lisp-errors-p*
                      (log-message* *lisp-errors-log-level* "~A~@[~%~A~]" error (when *log-lisp-backtraces-p*
                                                                                  backtrace)))
                    (start-output +http-internal-server-error+
                                  (acceptor-status-message *acceptor*
                                                           +http-internal-server-error+
                                                           :error (princ-to-string error)
                                                           :backtrace (princ-to-string backtrace)))))
               (multiple-value-bind (contents error backtrace)
                   ;; skip dispatch if bad request
                   (when (eql (return-code *reply*) +http-ok+)
                     (catch 'handler-done
                       (handle-request *acceptor* *request*)))
                 (when error
                   ;; error occurred in request handler
                   (report-error-to-client error backtrace))
                 (unless *headers-sent*
                   (handler-case
                       (with-debugger
                         (start-output (return-code *reply*)
                                       (or contents
                                           (acceptor-status-message *acceptor*
                                                                    (return-code *reply*)))))
                     (error (e)
                       ;; error occurred while writing to the client.  attempt to report.
                       (report-error-to-client e)))))))
        (dolist (path *tmp-files*)
          (when (and (pathnamep path) (probe-file path))
            ;; the handler may have chosen to (re)move the uploaded
            ;; file, so ignore errors that happen during deletion
            (ignore-errors*
              (delete-file path))))))))

(defun within-request-p ()
  "True if we're in the context of a request, otherwise nil."
  (and (boundp '*request*) *request*))

(defun parse-multipart-form-data (request external-format)
  "Parse the REQUEST body as multipart/form-data, assuming that its
content type has already been verified.  Returns the form data as
alist or NIL if there was no data or the data could not be parsed."
  (handler-case*
      (let* ((content-length (header-in :content-length request))
             (content-stream (make-flexi-stream (content-stream request)
                                               :external-format +latin-1+
                                               :bound (if content-length 
                                                        (parse-integer content-length 
                                                                       :junk-allowed T)))))
        (prog1
            (parse-rfc2388-form-data content-stream (header-in :content-type request) external-format)
          (let ((stray-data (get-post-data :already-read (flexi-stream-position content-stream))))
            (when (and stray-data (plusp (length stray-data)))
              (hunchentoot-warn "~A octets of stray data after form-data sent by client."
                                (length stray-data))))))
    (error (condition)
      (log-message* :error "While parsing multipart/form-data parameters: ~A" condition)
      nil)))

(defun maybe-read-post-parameters (&key (request *request*) force external-format)
  "Make surce that any POST parameters in the REQUEST are parsed.  The
body of the request must be either application/x-www-form-urlencoded
or multipart/form-data to be considered as containing POST parameters.
If FORCE is true, parsing is done unconditionally.  Otherwise, parsing
will only be done if the RAW-POST-DATA slot in the REQUEST is false.
EXTERNAL-FORMAT specifies the external format of the data in the
request body.  By default, the encoding is determined from the
Content-Type header of the request or from
*HUNCHENTOOT-DEFAULT-EXTERNAL-FORMAT* if none is found."
  (let* ((content-length (header-in :content-length request)))
    (when (and (header-in :content-type request)
               (member (request-method request) *methods-for-post-parameters* :test #'eq)
               (or force
                   (not (slot-value request 'raw-post-data)))
               ;; can't reparse multipart posts, even when FORCEd
               (not (eq t (slot-value request 'raw-post-data))))
      (unless (or content-length
                  (input-chunking-p))
        (log-message* :warning "Can't read request body because there's ~
no Content-Length header and input chunking is off.")
        (return-from maybe-read-post-parameters nil))
      (handler-case*
        (multiple-value-bind (type subtype charset)
            (parse-content-type (header-in :content-type request))
          (let ((external-format (or external-format
                                     (when charset
                                       (handler-case
                                           (make-external-format charset :eol-style :lf)
                                         (error ()
                                           (hunchentoot-warn "Ignoring ~
unknown character set ~A in request content type."
                                                 charset))))
                                       *hunchentoot-default-external-format*)))
            (setf (slot-value request 'post-parameters)
                  (cond ((and (string-equal type "application")
                              (string-equal subtype "x-www-form-urlencoded"))
                         (form-url-encoded-list-to-alist
                          (split "&" (raw-post-data :request request :external-format +latin-1+))
                          external-format))
                        ((and (string-equal type "multipart")
                              (string-equal subtype "form-data")
                              (if content-length 
                                  (plusp (parse-integer content-length 
                                                    :junk-allowed T))
                                  T))
                         (prog1 (parse-multipart-form-data request external-format)
                           (setf (slot-value request 'raw-post-data) t)))))))
          (error (condition)
                 (log-message* :error "Error when reading POST parameters from body: ~A" condition)
                 ;; this is not the right thing to do because it could happen
                 ;; that we aren't finished reading from the request stream and
                 ;; can't send a reply - to be revisited
                 (setf (return-code*) +http-bad-request+
                       *finish-processing-socket* t)
                 (abort-request-handler))))))

(defun recompute-request-parameters (&key (request *request*)
                                          (external-format *hunchentoot-default-external-format*))
  "Recomputes the GET and POST parameters for the REQUEST object
REQUEST.  This only makes sense if you're switching external formats
during the request."
  (maybe-read-post-parameters :request request :force t :external-format external-format)
  (setf (slot-value request 'get-parameters)
        (form-url-encoded-list-to-alist (split "&" (query-string request)) external-format))
  (values))
                                                
(defun script-name* (&optional (request *request*))
  "Returns the file name of the REQUEST object REQUEST. That's the
requested URI without the query string \(i.e the GET parameters)."
  (script-name request))

(defun query-string* (&optional (request *request*))
  "Returns the query string of the REQUEST object REQUEST. That's
the part behind the question mark \(i.e. the GET parameters)."
  (query-string request))

(defun get-parameters* (&optional (request *request*))
  "Returns an alist of the GET parameters associated with the REQUEST
object REQUEST."
  (get-parameters request))

(defmethod post-parameters :before ((request request))
  ;; Force here because if someone calls POST-PARAMETERS they actually
  ;; want them, regardless of why the RAW-POST-DATA has been filled
  ;; in. (For instance, if SEND-HEADERS has been called, filling in
  ;; RAW-POST-DATA, and then subsequent code calls POST-PARAMETERS,
  ;; without the :FORCE flag POST-PARAMETERS would return NIL.)
  (maybe-read-post-parameters
   :request request :force (not (slot-value request 'post-parameters))))

(defun post-parameters* (&optional (request *request*))
  "Returns an alist of the POST parameters associated with the REQUEST
object REQUEST."
  (post-parameters request))

(defun headers-in* (&optional (request *request*))
  "Returns an alist of the incoming headers associated with the
REQUEST object REQUEST."
  (headers-in request))

(defun cookies-in* (&optional (request *request*))
  "Returns an alist of all cookies associated with the REQUEST object
REQUEST."
  (cookies-in request))

(defgeneric header-in (name request)
  (:documentation "Returns the incoming header with name NAME.  NAME
can be a keyword \(recommended) or a string.")
  (:method (name request)
   (cdr (assoc* name (headers-in request)))))

(defun header-in* (name &optional (request *request*))
  "Returns the incoming header with name NAME.  NAME can be a keyword
\(recommended) or a string."
  (header-in name request))

(defun authorization (&optional (request *request*))
  "Returns as two values the user and password \(if any) as encoded in
the 'AUTHORIZATION' header.  Returns NIL if there is no such header."
  (let* ((authorization (header-in :authorization request))
         (start (and authorization
                     (> (length authorization) 5)
                     (string-equal "Basic" authorization :end2 5)
                     (scan "\\S" authorization :start 5))))
    (when start
      (destructuring-bind (&optional user password)
          (split ":" (base64:base64-string-to-string (subseq authorization start)) :limit 2)
        (values user password)))))

(defun remote-addr* (&optional (request *request*))
  "Returns the address the current request originated from."
  (remote-addr request))

(defun remote-port* (&optional (request *request*))
  "Returns the port the current request originated from."
  (remote-port request))

(defun local-addr* (&optional (request *request*))
  "Returns the address the current request connected to."
  (local-addr request))

(defun local-port* (&optional (request *request*))
  "Returns the port the current request connected to."
  (local-port request))

(defun real-remote-addr (&optional (request *request*))
  "Returns the 'X-Forwarded-For' incoming http header as the
second value in the form of a list of IP addresses and the first
element of this list as the first value if this header exists.
Otherwise returns the value of REMOTE-ADDR as the only value."
  (let ((x-forwarded-for (header-in :x-forwarded-for request)))
    (cond (x-forwarded-for (let ((addresses (split "\\s*,\\s*" x-forwarded-for)))
                             (values (first addresses) addresses)))
          (t (remote-addr request)))))

(defun host (&optional (request *request*))
  "Returns the 'Host' incoming http header value."
  (header-in :host request))

(defun request-uri* (&optional (request *request*))
  "Returns the request URI."
  (request-uri request))

(defun request-method* (&optional (request *request*))
  "Returns the request method as a Lisp keyword."
  (request-method request))

(defun server-protocol* (&optional (request *request*))
  "Returns the request protocol as a Lisp keyword."
  (server-protocol request))

(defun user-agent (&optional (request *request*))
  "Returns the 'User-Agent' http header."
  (header-in :user-agent request))

(defun cookie-in (name &optional (request *request*))
  "Returns the cookie with the name NAME \(a string) as sent by the
browser - or NIL if there is none."
  (cdr (assoc name (cookies-in request) :test #'string=)))

(defun referer (&optional (request *request*))
  "Returns the 'Referer' \(sic!) http header."
  (header-in :referer request))

(defun get-parameter (name &optional (request *request*))
  "Returns the GET parameter with name NAME \(a string) - or NIL if
there is none.  Search is case-sensitive."
  (cdr (assoc name (get-parameters request) :test #'string=)))

(defun post-parameter (name &optional (request *request*))
  "Returns the POST parameter with name NAME \(a string) - or NIL if
there is none.  Search is case-sensitive."
  (cdr (assoc name (post-parameters request) :test #'string=)))

(defun parameter (name &optional (request *request*))
  "Returns the GET or the POST parameter with name NAME \(a string) -
or NIL if there is none.  If both a GET and a POST parameter with the
same name exist the GET parameter is returned.  Search is
case-sensitive."
  (or (get-parameter name request)
      (post-parameter name request)))

(defun handle-if-modified-since (time &optional (request *request*))
  "Handles the 'If-Modified-Since' header of REQUEST.  The date string
is compared to the one generated from the supplied universal time
TIME."
  (let ((if-modified-since (header-in :if-modified-since request))
        (time-string (rfc-1123-date time)))
    ;; simple string comparison is sufficient; see RFC 2616 14.25
    (when (and if-modified-since
               (equal if-modified-since time-string))
      (setf (slot-value *reply* 'content-length) nil
	    (slot-value *reply* 'headers-out) (remove :content-length (headers-out*) :key #'car)
	    (return-code*) +http-not-modified+)
      (abort-request-handler))
    (values)))

(defun external-format-from-content-type (content-type)
  "Creates and returns an external format corresponding to the value
of the content type header provided in CONTENT-TYPE.  If the content
type was not set or if the character set specified was invalid, NIL is
returned."
  (when content-type
    (when-let (charset (nth-value 2 (parse-content-type content-type)))
      (handler-case
          (make-external-format (as-keyword charset) :eol-style :lf)
        (error ()
          (hunchentoot-warn "Invalid character set ~S in request has been ignored."
                            charset))))))

(defun raw-post-data (&key (request *request*) external-format force-text force-binary want-stream)
  "Returns the content sent by the client if there was any \(unless
the content type was \"multipart/form-data\").  By default, the result
is a string if the type of the `Content-Type' media type is \"text\",
and a vector of octets otherwise.  In the case of a string, the
external format to be used to decode the content will be determined
from the `charset' parameter sent by the client \(or otherwise
*HUNCHENTOOT-DEFAULT-EXTERNAL-FORMAT* will be used).

You can also provide an external format explicitly \(through
EXTERNAL-FORMAT) in which case the result will unconditionally be a
string.  Likewise, you can provide a true value for FORCE-TEXT which
will force Hunchentoot to act as if the type of the media type had
been \"text\".  Or you can provide a true value for FORCE-BINARY which
means that you want a vector of octets at any rate.

If, however, you provide a true value for WANT-STREAM, the other
parameters are ignored and you'll get the content \(flexi) stream to
read from it yourself.  It is then your responsibility to read the
correct amount of data, because otherwise you won't be able to return
a response to the client.  If the content type of the request was
`multipart/form-data' or `application/x-www-form-urlencoded', the
content has been read by Hunchentoot already and you can't read from
the stream anymore.

You can call RAW-POST-DATA more than once per request, but you can't
mix calls which have different values for WANT-STREAM.

Note that this function is slightly misnamed because a client can send
content even if the request method is not POST."
  (when (and force-binary force-text)
    (parameter-error "It doesn't make sense to set both FORCE-BINARY and FORCE-TEXT to a true value."))
  (unless (or external-format force-binary)
    (setq external-format (or (external-format-from-content-type (header-in :content-type request))
                              (when force-text
                                *hunchentoot-default-external-format*))))
  (let ((raw-post-data (or (slot-value request 'raw-post-data)
                           (get-post-data :request request :want-stream want-stream))))
    (cond ((typep raw-post-data 'stream) raw-post-data)
          ((member raw-post-data '(t nil)) nil)
          (external-format (octets-to-string raw-post-data :external-format external-format))
          (t raw-post-data))))

(defun aux-request-value (symbol &optional (request *request*))
  "Returns the value associated with SYMBOL from the request object
REQUEST \(the default is the current request) if it exists.  The
second return value is true if such a value was found."
  (when request
    (let ((found (assoc symbol (aux-data request) :test #'eq)))
      (values (cdr found) found))))

(defsetf aux-request-value (symbol &optional request)
    (new-value)
  "Sets the value associated with SYMBOL from the request object
REQUEST \(default is *REQUEST*).  If there is already a value
associated with SYMBOL it will be replaced."
  (once-only (symbol)
    (with-gensyms (place %request)
      `(let* ((,%request (or ,request *request*))
              (,place (assoc ,symbol (aux-data ,%request) :test #'eq)))
         (cond
           (,place
            (setf (cdr ,place) ,new-value))
           (t
            (push (cons ,symbol ,new-value)
                  (aux-data ,%request))
            ,new-value))))))

(defun delete-aux-request-value (symbol &optional (request *request*))
  "Removes the value associated with SYMBOL from the request object
REQUEST."
  (when request
    (setf (aux-data request)
            (delete symbol (aux-data request)
                    :key #'car :test #'eq)))
  (values))

(defun parse-path (path)
  "Return a relative pathname that has been verified to not contain
  any directory traversals or explicit device or host fields.  Returns
  NIL if the path is not acceptable."
  (when (every #'graphic-char-p path)
    (let* ((pathname (#+sbcl sb-ext:parse-native-namestring
                      #+ccl ccl:native-to-pathname
                      ;; Just disallow anything with :wild components later.
                      #-(or ccl sbcl) parse-namestring
                      (remove #\\ (regex-replace "^/*" path ""))))
           (directory (pathname-directory pathname)))
      (when (and (or (null (pathname-host pathname))
                     (equal (pathname-host pathname)
                            (pathname-host *default-pathname-defaults*)))
                 (or (null (pathname-device pathname))
                     (equal (pathname-device pathname)
                            (pathname-device *default-pathname-defaults*)))
                 (or (null directory)
                     (and (eql (first directory) :relative)
                          ;; only string components, no :UP traversals or :WILD
                          (every #'stringp (rest directory))))
                 #-(or sbcl ccl) ;; parse-native-namestring should handle this
                 (and
                  (typep (pathname-name pathname) '(or null string)) ; no :WILD
                  (typep (pathname-type pathname) '(or null string)))
                 (not (equal (file-namestring pathname) "..")))
        pathname))))

(defun request-pathname (&optional (request *request*) drop-prefix)
  "Construct a relative pathname from the request's SCRIPT-NAME.
If DROP-PREFIX is given, pathname construction starts at the first path
segment after the prefix.
"
  (let ((path (script-name request)))
    (if drop-prefix
        (when (starts-with-p path drop-prefix)
          (parse-path (subseq path (length drop-prefix))))
        (parse-path path))))
