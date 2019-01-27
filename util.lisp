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


(defun starts-with-p (seq subseq &key (test 'eql))
  "Tests whether the sequence SEQ starts with the sequence
SUBSEQ.  Individual elements are compared with TEST."
  (let* ((length (length subseq))
         (mismatch (mismatch subseq seq
                             :test test)))
    (or (null mismatch)
        (<= length mismatch))))

(defun starts-with-one-of-p (seq subseq-list &key (test 'eql))
  "Tests whether the sequence SEQ starts with one of the
sequences in SUBSEQ-LIST.  Individual elements are compared with
TEST."
  (some (lambda (subseq)
          (starts-with-p seq subseq :test test))
        subseq-list))

(defun create-random-string (&optional (n 10) (base 16))
  "Returns a random number \(as a string) with base BASE and N
digits."
  (with-output-to-string (s)
    (dotimes (i n)
      (format s "~VR" base
              (random base *the-random-state*)))))

(defun reason-phrase (return-code)
  "Returns a reason phrase for the HTTP return code RETURN-CODE \(which
should be an integer) or NIL for return codes Hunchentoot doesn't know."
  (gethash return-code *http-reason-phrase-map* 
           "No reason phrase known"))

(defgeneric assoc* (thing alist)
  (:documentation "Similar to CL:ASSOC, but 'does the right thing' if
THING is a string or a symbol.")
  (:method ((thing symbol) alist)
   (assoc thing alist :test #'eq))
  (:method ((thing string) alist)
   (assoc thing alist :test #'string-equal))
  (:method (thing alist)
   (assoc thing alist :test #'eql)))

(defun md5-hex (string)
  "Calculates the md5 sum of the string STRING and returns it as a hex string."
  (with-output-to-string (s)
    (loop for code across (md5:md5sum-string string)
	  do (format s "~2,'0x" code))))

(defun escape-for-html (string)
  "Escapes the characters #\\<, #\\>, #\\', #\\\", and #\\& for HTML
output."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            do (case char
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
                 ((#\") (write-string "&quot;" out))
                 ((#\') (write-string "&#039;" out))
                 ((#\&) (write-string "&amp;" out))
                 (otherwise (write-char char out)))))))

(defun http-token-p (token)
  "This function tests whether OBJECT is a non-empty string which is a
TOKEN according to RFC 2068 \(i.e. whether it may be used for, say,
cookie names)."
  (and (stringp token)
       (plusp (length token))
       (every (lambda (char)
                (and ;; CHAR is US-ASCII but not control character or ESC
                     (< 31 (char-code char) 127)
                     ;; CHAR is not 'tspecial'
                     (not (find char "()<>@,;:\\\"/[]?={} " :test #'char=))))
              token)))


(defun rfc-1123-date (&optional (time (get-universal-time)))
  "Generates a time string according to RFC 1123. Default is current time.
This can be used to send a 'Last-Modified' header - see
HANDLE-IF-MODIFIED-SINCE."
  (multiple-value-bind
        (second minute hour date month year day-of-week)
      (decode-universal-time time 0)
    (format nil "~A, ~2,'0d ~A ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
            (svref +day-names+ day-of-week)
            date
            (svref +month-names+ (1- month))
            year
            hour
            minute
            second)))

(defun iso-time (&optional (time (get-universal-time)))
  "Returns the universal time TIME as a string in full ISO format."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))

(let ((counter 0))
  (declare (ignorable counter))
  (defun make-tmp-file-name (&optional (prefix "hunchentoot"))
    "Generates a unique name for a temporary file.  This function is
called from the RFC2388 library when a file is uploaded."
    (let ((tmp-file-name
           #+:allegro
           (pathname (system:make-temp-file-name prefix *tmp-directory*))
           #-:allegro
           (loop for pathname = (make-pathname :name (format nil "~A-~A"
                                                             prefix (incf counter))
                                               :type nil
                                               :defaults *tmp-directory*)
                 unless (probe-file pathname)
                 return pathname)))
      (push tmp-file-name *tmp-files*)
      ;; maybe call hook for file uploads
      (when *file-upload-hook*
        (funcall *file-upload-hook* tmp-file-name))
      tmp-file-name)))

(defun quote-string (string)
  "Quotes string according to RFC 2616's definition of `quoted-string'."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            unless (or (char< char #\Space)
                       (char= char #\Rubout))
              do (case char
                   ((#\\) (write-string "\\\\" out))
                   ((#\") (write-string "\\\"" out))
                   (otherwise (write-char char out)))))))

(defmacro upgrade-vector (vector new-type &key converter)
  "Returns a vector with the same length and the same elements as
VECTOR \(a variable holding a vector) but having element type
NEW-TYPE.  If CONVERTER is not NIL, it should designate a function
which will be applied to each element of VECTOR before the result is
stored in the new vector.  The resulting vector will have a fill
pointer set to its end.

The macro also uses SETQ to store the new vector in VECTOR."
  `(setq ,vector
         (loop with length = (length ,vector)
               with new-vector = (make-array length
                                             :element-type ,new-type
                                             :fill-pointer length)
               for i below length
               do (setf (aref new-vector i) ,(if converter
                                               `(funcall ,converter (aref ,vector i))
                                               `(aref ,vector i)))
               finally (return new-vector))))

(defun ensure-parse-integer (string &key (start 0) end (radix 10))
  (let ((end (or end (length string))))
    (if (or (>= start (length string))
            (> end (length string)))
        (error 'bad-request)
        (multiple-value-bind (integer stopped)
            (parse-integer string :start start :end end :radix radix :junk-allowed t)
          (if (/= stopped end)
              (error 'bad-request)
              integer)))))

(defun url-decode (string &optional (external-format *hunchentoot-default-external-format*)
                                    (decode-plus t))
  "Decodes a URL-encoded string which is assumed to be encoded using the
external format EXTERNAL-FORMAT, i.e. this is the inverse of
URL-ENCODE. It is assumed that you'll rarely need this function, if
ever. But just in case - here it is. The default for EXTERNAL-FORMAT is
the value of *HUNCHENTOOT-DEFAULT-EXTERNAL-FORMAT*."
  (when (zerop (length string))
    (return-from url-decode ""))
  (let ((vector (make-array (length string) :element-type 'octet :fill-pointer 0))
        (i 0)
        unicodep)
    (loop
      (unless (< i (length string))
        (return))
      (let ((char (aref string i)))
       (labels ((decode-hex (length)
                  (ensure-parse-integer string :start i :end (incf i length)
                                               :radix 16))
                (push-integer (integer)
                  (vector-push integer vector))
                (peek ()
                  (if (array-in-bounds-p string i)
                      (aref string i)
                      (error 'bad-request)))
                (advance ()
                  (setq char (peek))
                  (incf i)))
         (cond
          ((char= #\% char)
           (advance)
           (cond
            ((char= #\u (peek))
             (unless unicodep
               (setq unicodep t)
               (upgrade-vector vector '(integer 0 65535)))
             (advance)
             (push-integer (decode-hex 4)))
            (t
             (push-integer (decode-hex 2)))))
          (t
           (push-integer (char-code (case char
                                      ((#\+)
                                       (if decode-plus
                                           #\Space
                                           char))
                                      (otherwise char))))
           (advance))))))
    (cond (unicodep
           (upgrade-vector vector 'character :converter #'code-char))
          (t (octets-to-string vector :external-format external-format)))))

(defun form-url-encoded-list-to-alist (form-url-encoded-list
                                       &optional (external-format *hunchentoot-default-external-format*))
  "Converts a list FORM-URL-ENCODED-LIST of name/value pairs into an
alist.  Both names and values are url-decoded while doing this."
  (mapcar #'(lambda (entry)
              (destructuring-bind (name &optional value)
                  (split "=" entry :limit 2)
                (cons (string-trim " " (url-decode name external-format))
                      (url-decode (or value "") external-format))))
          form-url-encoded-list))

(defun cookies-to-alist (cookies)
  "Converts a list of cookies of the form \"key=value\" to an alist.  No
  character set processing is done."
  (mapcar #'(lambda (entry)
              (destructuring-bind (name &optional value)
                  (split "=" entry :limit 2)
                (cons (string-trim " " name) (or value ""))))
          cookies))

(defun url-encode (string &optional (external-format *hunchentoot-default-external-format*))
  "URL-encodes a string using the external format EXTERNAL-FORMAT. The
default for EXTERNAL-FORMAT is the value of
*HUNCHENTOOT-DEFAULT-EXTERNAL-FORMAT*."
  (with-output-to-string (s)
    (loop for c across string
          for index from 0
          do (cond ((or (char<= #\0 c #\9)
                        (char<= #\a c #\z)
                        (char<= #\A c #\Z)
                        ;; note that there's no comma in there - because of cookies
                        (find c "$-_.!*'()" :test #'char=))
                     (write-char c s))
                   (t (loop for octet across (string-to-octets string
                                                               :start index
                                                               :end (1+ index)
                                                               :external-format external-format)
                            do (format s "%~2,'0x" octet)))))))

(defun parse-content-type (content-type-header)
  "Reads and parses a `Content-Type' header and returns it as three
values - the type, the subtype, and the requests' character set as
specified in the 'charset' parameter in the header, if there is one
and if the content type is \"text\".  CONTENT-TYPE-HEADER is supposed
to be the corresponding header value as a string."
  (with-input-from-sequence (stream (map 'list 'char-code content-type-header))
    (with-character-stream-semantics
     (let* ((*current-error-message* (format nil "Corrupted Content-Type header ~S:" content-type-header))
            (type (read-token stream))
            (subtype (if (eql #\/ (read-char* stream nil))
                       (read-token stream)
                       (return-from parse-content-type
                         ;; try to return something meaningful
                         (values "application" "octet-stream" nil))))
            (parameters (read-name-value-pairs stream))
            (charset (cdr (assoc "charset" parameters :test #'string=)))
            (charset
             (when (string-equal type "text")
               charset)))
       (values type subtype charset)))))

(defun keep-alive-p (request)
  "Returns a true value unless the incoming request's headers or the
server's PERSISTENT-CONNECTIONS-P setting obviate a keep-alive reply.
The second return value denotes whether the client has explicitly
asked for a persistent connection."
  (let ((connection-values
         ;; the header might consist of different values separated by commas
         (when-let (connection-header (header-in :connection request))
           (split "\\s*,\\s*" connection-header))))
    (flet ((connection-value-p (value)
             "Checks whether the string VALUE is one of the
values of the `Connection' header."
             (member value connection-values :test #'string-equal)))
      (let ((keep-alive-requested-p (connection-value-p "keep-alive")))
        (values (and (acceptor-persistent-connections-p *acceptor*)
                     (or (and (eq (server-protocol request) :http/1.1)
                              (not (connection-value-p "close")))
                         (and (eq (server-protocol request) :http/1.0)
                              keep-alive-requested-p)))
                keep-alive-requested-p)))))

(defun address-string ()
  "Returns a string with information about Hunchentoot suitable for
inclusion in HTML output."
  (flet ((escape-for-html (arg)
           (if arg
               (escape-for-html arg)
               arg)))
    (format nil "<address><a href='http://weitz.de/hunchentoot/'>Hunchentoot ~A</a> <a href='~A'>(~A ~A)</a>~@[ at ~A~:[ (port ~D)~;~]~]</address>"
            *hunchentoot-version*
            +implementation-link+
            (escape-for-html (lisp-implementation-type))
            (escape-for-html (lisp-implementation-version))
            (escape-for-html (or (host *request*) (acceptor-address *acceptor*)))
            (scan ":\\d+$" (or (host *request*) ""))
            (acceptor-port *acceptor*))))

(defun input-chunking-p ()
  "Whether input chunking is currently switched on for
*HUNCHENTOOT-STREAM* - note that this will return NIL if the stream
not a chunked stream."
  (chunked-stream-input-chunking-p *hunchentoot-stream*))

(defun ssl-p (&optional (acceptor *acceptor*))
  "Whether the current connection to the client is secure. See
ACCEPTOR-SSL-P."
  (acceptor-ssl-p acceptor))

(defmacro with-mapped-conditions (() &body body)
  "Run BODY with usocket condition mapping in effect, i.e. platform specific network errors will be
  signalled as usocket conditions.  For Lispworks, no mapping is performed."
  #+:lispworks
  `(progn ,@body)
  #-:lispworks
  `(usocket:with-mapped-conditions ()
    ,@body))

(defmacro with-conditions-caught-and-logged (() &body body)
  "Run BODY with conditions caught and logged by the *ACCEPTOR*. Errors are
stopped right away so no other part of the software is impacted by them."
  `(block nil
     (handler-bind
         ((error
           ;; abort if there's an error which isn't caught inside
           (lambda (cond)
             (log-message* *lisp-errors-log-level*
                           "Error while processing connection: ~A" cond)
             (return)))
          (warning
           ;; log all warnings which aren't caught inside
           (lambda (cond)
             (when *log-lisp-warnings-p*
               (log-message* *lisp-warnings-log-level*
                             "Warning while processing connection: ~A" cond)))))
       ,@body)))
