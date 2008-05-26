;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/headers.lisp,v 1.29 2008/03/27 08:08:31 edi Exp $

;;; Copyright (c) 2004-2008, Dr. Edmund Weitz.  All rights reserved.

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

(defun maybe-write-to-header-stream (key &optional value)
  (when *header-stream*
    (format *header-stream* "~A~@[: ~A~]~%" key
            (and value (regex-replace-all "[\\r\\n]" value " ")))
    (force-output *header-stream*)))

(defgeneric write-header-line (key value)
  (:documentation "Accepts strings KEY and VALUE and writes them
directly to the client as a HTTP header line.")
  (:method (key (string string))
    (let ((stream (flexi-stream-stream *hunchentoot-stream*)))
      (labels ((write-header-char (char)
                 (when *header-stream*
                   (write-char char *header-stream*))
                 (write-byte (char-code char) stream))
               (write-header-string (string &key (start 0) (end (length string)))
                 (loop for i from start below end
                       do (write-header-char (aref string i)))))
        (write-header-string key)
        (write-header-char #\:)
        (write-header-char #\Space)
        (let ((start 0))
          (loop
            (let ((end (or (position #\Newline string :start start)
                           (length string))))
              ;; skip empty lines, as they confuse certain HTTP clients
              (unless (eql start end)
                (unless (zerop start)
                  (write-header-char #\Tab))
                (write-header-string string :start start :end end)
                (write-header-char #\Return)
                (write-header-char #\Linefeed))
              (setf start (1+ end))
              (when (<= (length string) start)
                (return))))))))
  (:method (key value)
    (write-header-line key (princ-to-string value))))

#-with-step-instrumentation
(defmacro with-step-instrumentation ((report-label) &body body)
  (declare (ignore report-label))
  `(macrolet ((note-step (name)
                (declare (ignore name))))
     ,@body))

#+with-step-instrumentation
(defmacro with-step-instrumentation ((report-label) &body body)
  (with-unique-names (start-time stamps)
    `(let ((,start-time (prof::get-real-time))
           ,stamps)
       (macrolet ((note-step (name)
                    `(push (cons ',name (prof::get-real-time)) ,',stamps)))
         (multiple-value-prog1
             (progn ,@body)
           (format *trace-output* "step instrumentation for ~A~%" ,report-label)
           (dolist (stamp (nreverse ,stamps))
             (format *trace-output* "~A ~A~%" (car stamp) (prof::format-time (- (cdr stamp)
                                                                                ,start-time)))))))))

(defun start-output (&optional (content nil content-provided-p))
  "Sends all headers and maybe the content body to
*HUNCHENTOOT-STREAM*.  Returns immediately and does nothing if called
more than once per request.  Handles the supported return codes
accordingly.  Called by PROCESS-REQUEST and/or SEND-HEADERS.  Returns
the stream to write to."
  ;; send headers only once
  (when *headers-sent*
    (return-from start-output))
  (setq *headers-sent* t)
  ;; Read post data to clear stream - Force binary mode to avoid OCTETS-TO-STRING overhead.
  (raw-post-data :force-binary t)
  (let* ((return-code (return-code))
         (chunkedp (and (server-output-chunking-p *server*)
                        (eq (server-protocol) :http/1.1)
                        ;; only turn chunking on if the content
                        ;; length is unknown at this point...
                        (null (or (content-length) content-provided-p))
                        ;; ...AND if the return code isn't one where
                        ;; Hunchentoot (or a user error handler) sends its
                        ;; own content
                        (member return-code *approved-return-codes*)))
         (reason-phrase (reason-phrase return-code))
         (request-method (request-method))
         (head-request-p (eq request-method :head))
         content-modified-p)
    (multiple-value-bind (keep-alive-p keep-alive-requested-p)
        (keep-alive-p)
      (when keep-alive-p
        (setq keep-alive-p
              ;; use keep-alive if there's a way for the client to
              ;; determine when all content is sent (or if there
              ;; is no content)
              (or chunkedp
                  head-request-p
                  (eql (return-code) +http-not-modified+)
                  (content-length)
                  content)))
      ;; now set headers for keep-alive and chunking
      (when chunkedp
        (setf (header-out "Transfer-Encoding") "chunked"))
      (cond (keep-alive-p
             (setf *close-hunchentoot-stream* nil)
             (when (and (server-read-timeout *server*)
                        (or (not (eq (server-protocol) :http/1.1))
                            keep-alive-requested-p))
               ;; persistent connections are implicitly assumed for
               ;; HTTP/1.1, but we return a 'Keep-Alive' header if the
               ;; client has explicitly asked for one
               (setf (header-out "Connection") "Keep-Alive"
                     (header-out "Keep-Alive")
                     (format nil "timeout=~D" (server-read-timeout *server*)))))
            (t (setf (header-out "Connection") "Close"))))
    (unless (and (header-out-set-p "Server")
                 (null (header-out "Server")))
      (setf (header-out "Server") (or (header-out "Server")
                                      (server-name-header))))
    (setf (header-out "Date") (rfc-1123-date))
    (unless reason-phrase
      (setq content (escape-for-html
                     (format nil "Unknown http return code: ~A" return-code))
            content-modified-p t
            return-code +http-internal-server-error+
            reason-phrase (reason-phrase return-code)))
    (unless (or (not *handle-http-errors-p*)
                (member return-code *approved-return-codes*))
      ;; call error handler, if any - should return NIL if it can't
      ;; handle the error
      (let (error-handled-p)
        (when *http-error-handler*
          (setq error-handled-p (funcall *http-error-handler* return-code)
                content (or error-handled-p content)
                content-modified-p (or content-modified-p error-handled-p)))
        ;; handle common return codes other than 200, which weren't
        ;; handled by the error handler
        (unless error-handled-p
          (setf (content-type)
                "text/html; charset=iso-8859-1"
                content-modified-p t
                content
                (format nil "<html><head><title>~D ~A</title></head><body><h1>~:*~A</h1>~A<p><hr>~A</p></body></html>"
                        return-code reason-phrase
                        (case return-code
                          ((#.+http-internal-server-error+) content)
                          ((#.+http-moved-temporarily+ #.+http-moved-permanently+)
                           (format nil "The document has moved <a href='~A'>here</a>"
                                   (header-out "Location")))
                          ((#.+http-authorization-required+)
                           "The server could not verify that you are authorized to access the document requested.  Either you supplied the wrong credentials \(e.g., bad password), or your browser doesn't understand how to supply the credentials required.")
                          ((#.+http-forbidden+)
                           (format nil "You don't have permission to access ~A on this server."
                                   (script-name)))
                          ((#.+http-not-found+)
                           (format nil "The requested URL ~A was not found on this server."
                                   (script-name)))
                          ((#.+http-bad-request+)
                           "Your browser sent a request that this server could not understand.")
                          (otherwise ""))
                        (address-string))))))
    ;; start with status line      
    (let ((first-line
           (format nil "HTTP/1.1 ~D ~A" return-code reason-phrase)))
      (write-string first-line *hunchentoot-stream*)
      (write-string +crlf+ *hunchentoot-stream*)
      (maybe-write-to-header-stream first-line))
    (when (and (stringp content)
               (not content-modified-p)
               (starts-with-one-of-p (or (content-type) "")
                                     *content-types-for-url-rewrite*))
      ;; if the Content-Type header starts with one of the strings
      ;; in *CONTENT-TYPES-FOR-URL-REWRITE* then maybe rewrite the
      ;; content
      (setq content (maybe-rewrite-urls-for-session content)))
    (when (stringp content)
      ;; if the content is a string, convert it to the proper external format
      (setf content (string-to-octets content :external-format (reply-external-format))))
    (when content
      ;; whenever we know what we're going to send out as content, set
      ;; the content-length header properly.  It may be that the user
      ;; specified a different Content-Length, but that will not be
      ;; right.  We might want to warn the user.
      (setf (header-out :content-length) (length content)))
    ;; write all headers from the REPLY object
    (loop for (key . value) in (headers-out)
       when value
       do (write-header-line (string-capitalize key) value))
    ;; now the cookies
    (loop for (nil . cookie) in (cookies-out)
       do (write-header-line "Set-Cookie" (stringify-cookie cookie)))
    ;; all headers sent
    (write-string +crlf+ *hunchentoot-stream*)
    (maybe-write-to-header-stream "")
    ;; access log message
    (when (server-access-logger *server*)
      (funcall (server-access-logger *server*)
               :return-code return-code
               :content content
               :content-length (content-length)))
    (setf (flexi-stream-external-format *hunchentoot-stream*) (reply-external-format))
    ;; now optional content
    (unless (or (null content) head-request-p)
      #+:clisp
      (unless (stringp content)
        (setf (flexi-stream-element-type *hunchentoot-stream*) 'octet))
      (write-sequence content (typecase content
                                (string *hunchentoot-stream*)
                                ;; if the content is binary, we
                                ;; don't need FLEXI-STREAM's
                                ;; encoding capabilities
                                (otherwise (flexi-stream-stream *hunchentoot-stream*)))))
    (when chunkedp
      ;; turn chunking on after the headers have been sent
      (setf (chunked-stream-output-chunking-p
             (flexi-stream-stream *hunchentoot-stream*)) t))
    *hunchentoot-stream*))

(defun send-headers ()
  "Sends the initial status line and all headers as determined by
the REPLY object *REPLY*.  Returns a stream to which the body of
the reply can be written.  Once this function has been called,
further changes to *REPLY* don't have any effect.  Also,
automatic handling of errors \(i.e. sending the corresponding
status code to the browser, etc.) is turned off for this request.
If your handlers return the full body as a string or as an array
of octets you should NOT call this function."
  (start-output))

(defvar *break-even-while-reading-request-type-p* nil
  "If this variable is set to true, Hunchentoot will not bind
   *BREAK-ON-SIGNALS* to NIL while reading the next request type from
   an incoming connection.  By default, Hunchentoot will not enter the
   debugger if an error occurs during the reading of the request type,
   as this will happen regularily and legitimately (the incoming
   connection times out or the client closes the connection without
   initiating another request, which is permissible.")

(defun read-initial-request-line (stream)
  "Read and return the initial HTTP request line, catching permitted
errors and handling *BREAK-EVEN-WHILE-READING-REQUEST-TYPE-P*.  If no
request could be read, return NIL."
  (let ((*break-on-signals* (and *break-even-while-reading-request-type-p*
                                 *break-on-signals*)))
    (handler-case
        (read-line* stream)
      ((or end-of-file usocket:timeout-error) ()
        nil))))
  
(defun get-request-data (stream)
  "Reads incoming headers from the client via STREAM.  Returns as
multiple values the headers as an alist, the method, the URI, and the
protocol of the request."
  (let ((first-line (read-initial-request-line stream)))
    (when first-line
      (destructuring-bind (method url-string &optional protocol)
          (split "\\s+" first-line :limit 3)
        (maybe-write-to-header-stream first-line)
        (let ((headers (and protocol (read-http-headers stream
                                                        *header-stream*))))
          (unless protocol (setq protocol "HTTP/0.9"))
          (when (equalp (cdr (assoc :expect headers)) "100-continue")
            ;; handle 'Expect: 100-continue' header
            (let ((continue-line
                   (format nil "HTTP/1.1 ~D ~A"
                           +http-continue+
                           (reason-phrase +http-continue+))))
              (write-string continue-line stream)
              (write-string +crlf+ stream)
              (write-string +crlf+ stream)
              (force-output stream)
              (maybe-write-to-header-stream continue-line)
              (maybe-write-to-header-stream "")))
          (values headers
                  (make-keyword method)
                  url-string
                  (make-keyword (string-trim '(#\Space #\Tab #\NewLine #\Return) protocol))))))))
