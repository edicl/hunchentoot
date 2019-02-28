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

(let ((scanner-hash (make-hash-table :test #'equal)))
  (defun scanner-for-get-param (param-name)
    "Returns a CL-PPCRE scanner which matches a GET parameter in a
URL.  Scanners are memoized in SCANNER-HASH once they are created."
    (or (gethash param-name scanner-hash)
        (setf (gethash param-name scanner-hash)
                (create-scanner
                 `(:alternation
                   ;; session=value at end of URL
                   (:sequence
                    (:char-class #\? #\&)
                    ,param-name
                    #\=
                    (:greedy-repetition 0 nil (:inverted-char-class #\&))
                    :end-anchor)
                   ;; session=value with other parameters following
                   (:sequence
                    (:register (:char-class #\? #\&))
                    ,param-name
                    #\=
                    (:greedy-repetition 0 nil (:inverted-char-class #\&))
                    #\&))))))
  (defun add-cookie-value-to-url (url &key
                                      (cookie-name (session-cookie-name *acceptor*))
                                      (value (when-let (session (session *request*))
                                               (session-cookie-value session)))
                                      (replace-ampersands-p t))
    "Removes all GET parameters named COOKIE-NAME from URL and then
adds a new GET parameter with the name COOKIE-NAME and the value
VALUE.  If REPLACE-AMPERSANDS-P is true all literal ampersands in URL
are replaced with '&amp;'. The resulting URL is returned."
    (unless url
      ;; see URL-REWRITE:*URL-REWRITE-FILL-TAGS*
      (setq url (request-uri *request*)))
    (setq url (regex-replace-all (scanner-for-get-param cookie-name) url "\\1"))
    (when value
      (setq url (format nil "~A~:[?~;&~]~A=~A"
                        url
                        (find #\? url)
                        cookie-name
                        (url-encode value))))
    (when replace-ampersands-p
      (setq url (regex-replace-all "&" url "&amp;")))
    url))

(defun maybe-rewrite-urls-for-session (html &key
                                            (cookie-name (session-cookie-name *acceptor*))
                                            (value (when-let (session (session *request*))
                                                     (session-cookie-value session))))
  "Rewrites the HTML page HTML such that the name/value pair
COOKIE-NAME/COOKIE-VALUE is inserted if the client hasn't sent a
cookie of the same name but only if *REWRITE-FOR-SESSION-URLS* is
true.  See the docs for URL-REWRITE:REWRITE-URLS."
  (cond ((or (not *rewrite-for-session-urls*)
             (null value)
             (cookie-in cookie-name))
          html)
        (t
          (with-input-from-string (*standard-input* html)
            (with-output-to-string (*standard-output*)
              (url-rewrite:rewrite-urls
               (lambda (url)
                 (add-cookie-value-to-url url
                                          :cookie-name cookie-name
                                          :value value))))))))

(defun create-prefix-dispatcher (prefix handler)
  "Creates a request dispatch function which will dispatch to the
function denoted by HANDLER if the file name of the current request
starts with the string PREFIX."
  (lambda (request)
    (let ((mismatch (mismatch (script-name request) prefix
                              :test #'char=)))
      (and (or (null mismatch)
               (>= mismatch (length prefix)))
           handler))))

(defun create-regex-dispatcher (regex handler)
  "Creates a request dispatch function which will dispatch to the
function denoted by HANDLER if the file name of the current request
matches the CL-PPCRE regular expression REGEX."
  (let ((scanner (create-scanner regex)))
    (lambda (request)
      (and (scan scanner (script-name request))
           handler))))

(defun abort-request-handler (&optional result)
  "This function can be called by a request handler at any time to
immediately abort handling the request.  This works as if the handler
had returned RESULT.  See the source code of REDIRECT for an example."
  (throw 'handler-done result))

(defun maybe-handle-range-header (file)
  "Helper function for handle-static-file.  Determines whether the
  requests specifies a Range header.  If so, parses the header and
  position the already opened file to the location specified.  Returns
  the number of bytes to transfer from the file.  Invalid specified
  ranges are reported to the client with a HTTP 416 status code."
  (let ((bytes-to-send (file-length file)))
    (cl-ppcre:register-groups-bind
        (start end)
        ("^bytes=(\\d+)-(\\d*)$" (header-in* :range) :sharedp t)
      ;; body won't be executed if regular expression does not match
      (setf start (parse-integer start))
      (setf end (if (> (length end) 0)
                    (parse-integer end)
                    (1- (file-length file))))
      (when (or (< start 0)
                (>= end (file-length file)))
        (setf (return-code*) +http-requested-range-not-satisfiable+
              (header-out :content-range) (format nil "bytes 0-~D/~D" (1- (file-length file)) (file-length file)))
        (throw 'handler-done
          (format nil "invalid request range (requested ~D-~D, accepted 0-~D)"
                  start end (1- (file-length file)))))
      (file-position file start)
      (setf (return-code*) +http-partial-content+
            bytes-to-send (1+ (- end start))
            (header-out :content-range) (format nil "bytes ~D-~D/~D" start end (file-length file))))
    bytes-to-send))

(defun handle-static-file (pathname &optional content-type callback)
  "A function which acts like a Hunchentoot handler for the file
denoted by PATHNAME.  Sends a content type header corresponding to
CONTENT-TYPE or \(if that is NIL) tries to determine the content type
via the suffix of the file.
CALLBACK is run just before sending the file, and can be used
to set headers or check authorization;
arguments are the filename and the (guessed) content-type."
  (when (or (wild-pathname-p pathname)
            (not (fad:file-exists-p pathname))
            (fad:directory-exists-p pathname))
    ;; file does not exist
    (setf (return-code*) +http-not-found+)
    (abort-request-handler))
  (unless content-type
    (setf content-type (mime-type pathname)))
  (let ((time (or (file-write-date pathname)
                  (get-universal-time)))
        bytes-to-send)
    (setf (content-type*) (or (and content-type
                                   (maybe-add-charset-to-content-type-header content-type (reply-external-format*)))
                              "application/octet-stream")
          (header-out :last-modified) (rfc-1123-date time)
          (header-out :accept-ranges) "bytes")
    (handle-if-modified-since time)
    (unless (null callback)
      (funcall callback pathname content-type))
    (with-open-file (file pathname
                          :direction :input
                          :element-type 'octet)
      (setf bytes-to-send (maybe-handle-range-header file)
            (content-length*) bytes-to-send)
      (let ((out (send-headers))
            (buf (make-array +buffer-length+ :element-type 'octet)))
        (loop
           (when (zerop bytes-to-send)
             (return))
           (let* ((chunk-size (min +buffer-length+ bytes-to-send)))
             (unless (eql chunk-size (read-sequence buf file :end chunk-size))
               (error "can't read from input file"))
             (write-sequence buf out :end chunk-size)
             (decf bytes-to-send chunk-size)))
        (finish-output out)))))

(defun create-static-file-dispatcher-and-handler (uri path &optional content-type callback)
  "Creates and returns a request dispatch function which will dispatch
to a handler function which emits the file denoted by the pathname
designator PATH with content type CONTENT-TYPE if the SCRIPT-NAME of
the request matches the string URI.  If CONTENT-TYPE is NIL, tries to
determine the content type via the file's suffix.
See HANDLE-STATIC-FILE for CALLBACK."
  ;; the dispatcher
  (lambda (request)
    (when (string= (script-name request) uri)
      ;; the handler
      (lambda ()
        (handle-static-file path content-type callback)))))

(defun create-folder-dispatcher-and-handler (uri-prefix base-path &optional content-type callback)
  "Creates and returns a dispatch function which will dispatch to a
handler function which emits the file relative to BASE-PATH that is
denoted by the URI of the request relative to URI-PREFIX.  URI-PREFIX
must be a string ending with a slash, BASE-PATH must be a pathname
designator for an existing directory.  If CONTENT-TYPE is not NIL,
it'll be the content type used for all files in the folder.
See HANDLE-STATIC-FILE for CALLBACK."
  (unless (and (stringp uri-prefix)
               (plusp (length uri-prefix))
               (char= (char uri-prefix (1- (length uri-prefix))) #\/))
    (parameter-error "~S must be string ending with a slash." uri-prefix))
  (unless (fad:directory-pathname-p base-path)
    (parameter-error "~S is supposed to denote a directory." base-path))
  (flet ((handler ()
           (let ((request-path (request-pathname *request* uri-prefix)))
             (when (null request-path)
               (setf (return-code*) +http-forbidden+)
               (abort-request-handler))
             (handle-static-file (merge-pathnames request-path base-path)
                                  content-type
                                  callback))))
    (create-prefix-dispatcher uri-prefix #'handler)))

(defun no-cache ()
  "Adds appropriate headers to completely prevent caching on most browsers."
  (setf (header-out :expires)
          "Mon, 26 Jul 1997 05:00:00 GMT"
        (header-out :cache-control)
          "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"
        (header-out :pragma)
          "no-cache"
        (header-out :last-modified)
          (rfc-1123-date))
  (values))

(defun redirect (target &key (host (host *request*) host-provided-p)
                             port
                             (protocol (if (ssl-p) :https :http))
                             (add-session-id (not (or host-provided-p
                                                      (starts-with-scheme-p target)
                                                      (cookie-in (session-cookie-name *acceptor*)))))
                             (code +http-moved-temporarily+))
  "Redirects the browser to TARGET which should be a string.  If
TARGET is a full URL starting with a scheme, HOST, PORT and PROTOCOL
are ignored.  Otherwise, TARGET should denote the path part of a URL,
PROTOCOL must be one of the keywords :HTTP or :HTTPS, and the URL to
redirect to will be constructed from HOST, PORT, PROTOCOL, and TARGET.
Adds a session ID if ADD-SESSION-ID is true.  If CODE is a 3xx
redirection code, it will be sent as status code."
  (check-type code (integer 300 399))
  (let ((url (if (starts-with-scheme-p target)
               target
               (format nil "~A://~A~@[:~A~]~A"
                       (ecase protocol
                         ((:http) "http")
                         ((:https) "https"))
                       (if port
                         (first (ppcre:split ":" (or host "")))
                         host)
                       port target))))
    (when add-session-id
      (setq url (add-cookie-value-to-url url :replace-ampersands-p nil)))
    (setf (header-out :location) url
          (return-code*) code)
    (abort-request-handler)))

(defun require-authorization (&optional (realm "Hunchentoot"))
  "Sends back appropriate headers to require basic HTTP authentication
\(see RFC 2617) for the realm REALM."
  (setf (header-out :www-authenticate)
          (format nil "Basic realm=\"~A\"" (quote-string realm))
        (return-code *reply*)
          +http-authorization-required+)
  (abort-request-handler))
