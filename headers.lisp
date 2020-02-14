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

(defgeneric write-header-line (key value stream)
  (:documentation "Accepts a string KEY and a Lisp object VALUE and
writes them directly to the client as an HTTP header line.")
  (:method (key (string string) stream)
    (write-string key stream)
    (write-char #\: stream)
    (write-char #\Space stream)
    (let ((start 0))
      (loop
         (let ((end (or (position #\Newline string :start start)
                        (length string))))
           ;; skip empty lines, as they confuse certain HTTP clients
           (unless (eql start end)
             (unless (zerop start)
               (write-char #\Tab stream))
             (write-string string stream :start start :end end)
             (write-char #\Return stream)
             (write-char #\Linefeed stream))
           (setf start (1+ end))
           (when (<= (length string) start)
             (return))))))
  (:method (key (number number) stream)
    (write-header-line key (write-to-string number :escape nil :readably nil :base 10) stream))
  (:method (key value stream)
    (write-header-line key (princ-to-string value) stream)))

(defun maybe-add-charset-to-content-type-header (content-type external-format)
  "Given the contents of a CONTENT-TYPE header, add a charset=
  attribute describing the given EXTERNAL-FORMAT if no charset=
  attribute is already present and the content type is a text content
  type.  Returns the augmented content type."
  (if (and (cl-ppcre:scan "(?i)^text" content-type)
           (not (cl-ppcre:scan "(?i);\\s*charset=" content-type)))
      (format nil "~A; charset=~(~A~)" content-type (flex:external-format-name external-format))
      content-type))

(defun start-output (return-code &optional (content nil content-provided-p))
  "Sends all headers and maybe the content body to
*HUNCHENTOOT-STREAM*.  Returns immediately and does nothing if called
more than once per request.  Called by PROCESS-REQUEST and/or
SEND-HEADERS.  The RETURN-CODE argument represents the integer return
code of the request.  The corresponding reason phrase is determined by
calling the REASON-PHRASE function.  The CONTENT provided represents
the body data to send to the client, if any.  If it is not specified,
no body is written to the client.  The handler function is expected to
directly write to the stream in this case.

Returns the stream that is connected to the client."
  (let* ((chunkedp (and (acceptor-output-chunking-p *acceptor*)
                        (eq (server-protocol *request*) :http/1.1)
                        ;; only turn chunking on if the content
                        ;; length is unknown at this point...
                        (null (or (content-length*) content-provided-p))))
         (request-method (request-method *request*))
         (head-request-p (eq request-method :head))
         content-modified-p)
    (multiple-value-bind (keep-alive-p keep-alive-requested-p)
        (keep-alive-p *request*)
      (when keep-alive-p
        (setq keep-alive-p
              ;; use keep-alive if there's a way for the client to
              ;; determine when all content is sent (or if there
              ;; is no content)
              (or chunkedp
                  head-request-p
                  (eql (return-code*) +http-not-modified+)
                  (content-length*)
                  content)))
      ;; now set headers for keep-alive and chunking
      (when chunkedp
        (setf (header-out :transfer-encoding) "chunked"))
      (cond (keep-alive-p
             (setf *finish-processing-socket* nil)
             (when (and (acceptor-read-timeout *acceptor*)
                        (or (not (eq (server-protocol *request*) :http/1.1))
                            keep-alive-requested-p))
               ;; persistent connections are implicitly assumed for
               ;; HTTP/1.1, but we return a 'Keep-Alive' header if the
               ;; client has explicitly asked for one
               (unless (header-out :connection) ; allowing for handler overriding
                 (setf (header-out :connection) "Keep-Alive"))
               (setf (header-out :keep-alive)
                     (format nil "timeout=~D" (acceptor-read-timeout *acceptor*)))))
            ((not (header-out-set-p :connection))
             (setf (header-out :connection) "Close"))))
    (unless (and (header-out-set-p :server)
                 (null (header-out :server)))
      (setf (header-out :server) (or (header-out :server)
                                     (acceptor-server-name *acceptor*))))
    (setf (header-out :date) (rfc-1123-date))
    (when (and (stringp content)
               (not content-modified-p)
               (starts-with-one-of-p (or (content-type*) "")
                                     *content-types-for-url-rewrite*))
      ;; if the Content-Type header starts with one of the strings
      ;; in *CONTENT-TYPES-FOR-URL-REWRITE* then maybe rewrite the
      ;; content
      (setq content (maybe-rewrite-urls-for-session content)))
    (when (stringp content)
      ;; if the content is a string, convert it to the proper external format
      (setf content (string-to-octets content :external-format (reply-external-format*))
            (content-type*) (maybe-add-charset-to-content-type-header (content-type*)
                                                                      (reply-external-format*))))
    (when content
      ;; whenever we know what we're going to send out as content, set
      ;; the Content-Length header properly; maybe the user specified
      ;; a different content length, but that will wrong anyway
      (setf (header-out :content-length) (length content)))
    ;; send headers only once
    (when *headers-sent*
      (return-from start-output))
    (setq *headers-sent* t)
    (send-response *acceptor*
                   *hunchentoot-stream*
                   return-code
                   :headers (headers-out*)
                   :cookies (cookies-out*)
                   :content (unless head-request-p
                              content))
    ;; when processing a HEAD request, exit to return from PROCESS-REQUEST
    (when head-request-p
      (throw 'request-processed nil))
    (when chunkedp
      ;; turn chunking on after the headers have been sent
      (unless (typep *hunchentoot-stream* 'chunked-stream)
        (setq *hunchentoot-stream* (make-chunked-stream *hunchentoot-stream*)))
      (setf (chunked-stream-output-chunking-p *hunchentoot-stream*) t))
    *hunchentoot-stream*))

(defun send-response (acceptor stream status-code
                      &key headers cookies content)
  "Send a HTTP response to the STREAM and log the event in ACCEPTOR.
  STATUS-CODE is the HTTP status code used in the response.  HEADERS
  and COOKIES are used to create the response header.  If CONTENT is
  provided, it is sent as the response body.

  If *HEADER-STREAM* is not NIL, the response headers are written to
  that stream when they are written to the client.

  STREAM is returned."
  (when content
    (setf (content-length*) (length content)))
  (when (content-length*)
    (if (assoc :content-length headers)
        (setf (cdr (assoc :content-length headers)) (content-length*))
        (push (cons :content-length (content-length*)) headers)))
  ;; access log message
  (acceptor-log-access acceptor :return-code status-code)
  ;; Read post data to clear stream - Force binary mode to avoid OCTETS-TO-STRING overhead.
  (raw-post-data :force-binary t)
  (let* ((client-header-stream (flex:make-flexi-stream stream :external-format +latin-1+))
         (header-stream (if *header-stream*
                            (make-broadcast-stream *header-stream* client-header-stream)
                            client-header-stream)))
    ;; start with status line
    (format header-stream "HTTP/1.1 ~D ~A~C~C" status-code (reason-phrase status-code) #\Return #\Linefeed)
    ;; write all headers from the REPLY object
    (loop for (key . value) in headers
       when value
       do (write-header-line (as-capitalized-string key) value header-stream))
    ;; now the cookies
    (loop for (nil . cookie) in cookies
       do (write-header-line "Set-Cookie" (stringify-cookie cookie) header-stream))
    (format header-stream "~C~C" #\Return #\Linefeed))
  ;; now optional content
  (when content
    (write-sequence content stream)
    (finish-output stream))
  stream)

(defun send-headers ()
  "Sends the initial status line and all headers as determined by the
REPLY object *REPLY*.  Returns a binary stream to which the body of
the reply can be written.  Once this function has been called, further
changes to *REPLY* don't have any effect.  Also, automatic handling of
errors \(i.e. sending the corresponding status code to the browser,
etc.) is turned off for this request.  If your handlers return the
full body as a string or as an array of octets you should NOT call
this function.

This function does not return control to the caller during HEAD
request processing."
  (start-output (return-code*)))

(defun read-initial-request-line (stream)
  "Reads and returns the initial HTTP request line, catching permitted
errors and handling *BREAK-EVEN-WHILE-READING-REQUEST-TYPE-P*.  If no
request could be read, returns NIL.  At this point, both an
end-of-file as well as a timeout condition are normal; end-of-file
will occur when the client has decided to not send another request but
to close the connection instead, a timeout indicates that the
connection timeout established by Hunchentoot has expired and we do
not want to wait for another request any longer."
  (handler-case
      (let ((*current-error-message* "While reading initial request line:"))
        (with-mapped-conditions ()
          (read-line* stream)))
    ((or end-of-file #-:lispworks usocket:timeout-error) ())))

(defun send-bad-request-response (stream &optional additional-info)
  "Send a ``Bad Request'' response to the client."
  (write-sequence (flex:string-to-octets
                   (format nil "HTTP/1.0 ~D ~A~C~CConnection: close~C~C~C~CYour request could not be interpreted by this HTTP server~C~C~@[~A~]~C~C"
                           +http-bad-request+ (reason-phrase +http-bad-request+) #\Return #\Linefeed
                           #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed additional-info #\Return #\Linefeed))
                  stream))

(defun send-unknown-protocol-response (stream &optional additional-info)
  "Send a ``HTTP Version Not Supported'' response to the client."
  (write-sequence (flex:string-to-octets
                   (format nil "HTTP/1.0 ~D ~A~C~CConnection: close~C~C~C~CYour request could not be interpreted by this HTTP server~C~C~@[~A~]~C~C"
                           +http-version-not-supported+ (reason-phrase +http-version-not-supported+) #\Return #\Linefeed
                           #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed additional-info #\Return #\Linefeed))
                  stream))

(defun printable-ascii-char-p (char)
  (<= 32 (char-code char) 126))

(defconstant +valid-request-methods+
  #(:get :post :head :put :delete :connect :options :trace :patch))

(defconstant +valid-protocol-versions+ #(:http/1.0 :http/1.1))

(defun get-request-data (stream)
  "Reads incoming headers from the client via STREAM.  Returns as
multiple values the headers as an alist, the method, the URI, and the
protocol of the request."
  (with-character-stream-semantics
    (let ((first-line (read-initial-request-line stream)))
      (when first-line
        (unless (every #'printable-ascii-char-p first-line)
          (send-bad-request-response stream "Non-ASCII character in request line")
          (return-from get-request-data nil))
        (destructuring-bind (&optional method url-string protocol)
            (split "\\s+" first-line :limit 3)
          (cond ((not
                  (setf method
                        (find method +valid-request-methods+ :test #'string-equal)))
                 (send-bad-request-response stream)
                 (return-from get-request-data nil))
                ((not url-string)
                 (send-bad-request-response stream)
                 (return-from get-request-data nil))
                ((not protocol)
                 ;; HTTP/1.1 specifies that if protocol is not provided
                 ;; then assume protocol version to be 1.0
                 (setf protocol :http/1.0))
                ((not
                  (setf protocol
                        (find protocol +valid-protocol-versions+ :test #'string-equal)))
                 (send-unknown-protocol-response stream)
                 (return-from get-request-data nil)))
          (when *header-stream*
            (format *header-stream* "~A~%" first-line))
          (let ((headers (read-http-headers stream *header-stream*)))
            ;; maybe handle 'Expect: 100-continue' header
            (when-let (expectations (cdr (assoc* :expect headers)))
              (when (member "100-continue" (split "\\s*,\\s*" expectations) :test #'equalp)
                ;; according to 14.20 in the RFC - we should actually
                ;; check if we have to respond with 417 here
                (let ((continue-line
                        (format nil "HTTP/1.1 ~D ~A"
                                +http-continue+
                                (reason-phrase +http-continue+))))
                  (write-sequence (map 'list #'char-code continue-line) stream)
                  (write-sequence +crlf+ stream)
                  (write-sequence +crlf+ stream)
                  (force-output stream)
                  (when *header-stream*
                    (format *header-stream* "~A~%" continue-line)))))
            (values headers method url-string protocol)))))))
