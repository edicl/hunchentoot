;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/headers.lisp,v 1.29 2008/03/27 08:08:31 edi Exp $

;;; Copyright (c) 2004-2009, Dr. Edmund Weitz.  All rights reserved.

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
  "Accepts a string KEY and an optional Lisp object VALUE and writes
them directly to the character stream *HEADER-STREAM* as an HTTP
header line \(or as a simple line if VALUE is NIL)."
  (when *header-stream*
    (format *header-stream* "~A~@[: ~A~]~%" key
            (and value (regex-replace-all "[\\r\\n]" value " ")))
    (force-output *header-stream*)))

(defgeneric write-header-line (key value)
  (:documentation "Accepts a string KEY and a Lisp object VALUE and
writes them directly to the client as an HTTP header line.")
  (:method (key (string string))
    (let ((stream *hunchentoot-stream*))
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

(defun start-output (&key (content nil content-provided-p)
                          (request *request*))
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
  (let* ((return-code (return-code*))
         (chunkedp (and (acceptor-output-chunking-p *acceptor*)
                        (eq (server-protocol request) :http/1.1)
                        ;; only turn chunking on if the content
                        ;; length is unknown at this point...
                        (null (or (content-length*) content-provided-p))
                        ;; ...AND if the return code isn't one where
                        ;; Hunchentoot (or a user error handler) sends its
                        ;; own content
                        (member return-code *approved-return-codes*)))
         (reason-phrase (reason-phrase return-code))
         (request-method (request-method request))
         (head-request-p (eq request-method :head))
         content-modified-p)
    (multiple-value-bind (keep-alive-p keep-alive-requested-p)
        (keep-alive-p request)
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
             (setf *close-hunchentoot-stream* nil)
             (when (and (acceptor-read-timeout *acceptor*)
                        (or (not (eq (server-protocol request) :http/1.1))
                            keep-alive-requested-p))
               ;; persistent connections are implicitly assumed for
               ;; HTTP/1.1, but we return a 'Keep-Alive' header if the
               ;; client has explicitly asked for one
               (setf (header-out :connection) "Keep-Alive"
                     (header-out :keep-alive)
                     (format nil "timeout=~D" (acceptor-read-timeout *acceptor*)))))
            (t (setf (header-out :connection) "Close"))))
    (unless (and (header-out-set-p :server)
                 (null (header-out :server)))
      (setf (header-out :server) (or (header-out :server)
                                     (server-name-header))))
    (setf (header-out :date) (rfc-1123-date))
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
          (setf (content-type*)
                "text/html; charset=iso-8859-1"
                content-modified-p t
                content
                (format nil "<html><head><title>~D ~A</title></head><body><h1>~:*~A</h1>~A<p><hr>~A</p></body></html>"
                        return-code reason-phrase
                        (case return-code
                          ((#.+http-internal-server-error+) content)
                          ((#.+http-moved-temporarily+ #.+http-moved-permanently+)
                           (format nil "The document has moved <a href='~A'>here</a>"
                                   (header-out :location)))
                          ((#.+http-authorization-required+)
                           "The server could not verify that you are authorized to access the document requested.  Either you supplied the wrong credentials \(e.g., bad password), or your browser doesn't understand how to supply the credentials required.")
                          ((#.+http-forbidden+)
                           (format nil "You don't have permission to access ~A on this server."
                                   (script-name request)))
                          ((#.+http-not-found+)
                           (format nil "The requested URL ~A was not found on this server."
                                   (script-name request)))
                          ((#.+http-bad-request+)
                           "Your browser sent a request that this server could not understand.")
                          (otherwise ""))
                        (address-string))))))
    ;; start with status line      
    (let ((first-line
           (format nil "HTTP/1.1 ~D ~A" return-code reason-phrase)))
      (write-sequence (map 'list #'char-code first-line) *hunchentoot-stream*)
      (write-sequence +crlf+ *hunchentoot-stream*)
      (maybe-write-to-header-stream first-line))
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
      (setf content (string-to-octets content :external-format (reply-external-format*))))
    (when content
      ;; whenever we know what we're going to send out as content, set
      ;; the Content-Length header properly; maybe the user specified
      ;; a different content length, but that will wrong anyway
      (setf (header-out :content-length) (length content)))
    ;; write all headers from the REPLY object
    (loop for (key . value) in (headers-out*)
       when value
       do (write-header-line (as-capitalized-string key) value))
    ;; now the cookies
    (loop for (nil . cookie) in (cookies-out*)
       do (write-header-line "Set-Cookie" (stringify-cookie cookie)))
    ;; all headers sent
    (write-sequence +crlf+ *hunchentoot-stream*)
    (maybe-write-to-header-stream "")
    ;; access log message
    (when-let (access-logger (acceptor-access-logger *acceptor*))
      (funcall access-logger
               :return-code return-code
               :content content
               :content-length (content-length*)))
    ;; now optional content
    (unless (or (null content) head-request-p)
      (write-sequence content *hunchentoot-stream*))
    (when chunkedp
      ;; turn chunking on after the headers have been sent
      (unless (typep *hunchentoot-stream* 'chunked-stream)
        (setq *hunchentoot-stream* (make-chunked-stream *hunchentoot-stream*)))
      (setf (chunked-stream-output-chunking-p *hunchentoot-stream*) t))
    *hunchentoot-stream*))

(defun send-headers ()
  "Sends the initial status line and all headers as determined by the
REPLY object *REPLY*.  Returns a binary stream to which the body of
the reply can be written.  Once this function has been called, further
changes to *REPLY* don't have any effect.  Also, automatic handling of
errors \(i.e. sending the corresponding status code to the browser,
etc.) is turned off for this request.  If your handlers return the
full body as a string or as an array of octets you should NOT call
this function."
  (start-output))

(defun read-initial-request-line (stream)
  "Reads and returns the initial HTTP request line, catching permitted
errors and handling *BREAK-EVEN-WHILE-READING-REQUEST-TYPE-P*.  If no
request could be read, returns NIL."
  (let ((*break-on-signals* (and *break-even-while-reading-request-type-p*
                                 *break-on-signals*)))
    (handler-case
        (let ((*current-error-message* "While reading initial request line:"))
          (read-line* stream))
      ((or end-of-file #-:lispworks usocket:timeout-error) ()
        nil))))
  
(defun get-request-data (stream)
  "Reads incoming headers from the client via STREAM.  Returns as
multiple values the headers as an alist, the method, the URI, and the
protocol of the request."
  (with-character-stream-semantics
   (let ((first-line (read-initial-request-line stream)))
     (when first-line
       (destructuring-bind (method url-string &optional protocol)
           (split "\\s+" first-line :limit 3)
         (maybe-write-to-header-stream first-line)
         (let ((headers (and protocol (read-http-headers stream *header-stream*))))
           (unless protocol (setq protocol "HTTP/0.9"))
           (when (equalp (cdr (assoc :expect headers :test #'eq)) "100-continue")
             ;; handle 'Expect: 100-continue' header
             (let ((continue-line
                    (format nil "HTTP/1.1 ~D ~A"
                            +http-continue+
                            (reason-phrase +http-continue+))))
               (write-sequence (map 'list #'char-code continue-line) stream)
               (write-sequence +crlf+ stream)
               (write-sequence +crlf+ stream)
               (force-output stream)
               (maybe-write-to-header-stream continue-line)
               (maybe-write-to-header-stream "")))
           (values headers
                   (as-keyword method)
                   url-string
                   (as-keyword (trim-whitespace protocol)))))))))
