;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

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

(in-package :url-rewrite)

(declaim (inline skip-whitespace))
(defun skip-whitespace (&key (skip t) (write-through t))
  "Read characters from *STANDARD-INPUT* as long as they are
whitespace. Returns the string which was read unless SKIP is true. On
EOF the string read so far is returned. Writes all characters read to
*STANDARD-OUTPUT* if WRITE-THROUGH is true."
  (read-while #'whitespacep
              :skip skip
              :write-through write-through))

(defun read-delimited-string (&key (skip t) (write-through t))
  "Reads and returns as its first value a string from
*STANDARD-INPUT*. The string is either delimited by ' or \" in which
case the delimiters aren't part of the string but the second return
value is the delimiter character or it is assumed to extend to the
next character which is not a name constituent \(see NAME-CHAR-P). On
EOF the string read so far is returned. If SKIP is true NIL is
returned. Writes all characters read to *STANDARD-OUTPUT* if
WRITE-THROUGH is true."
  ;; note that this function has no means to signal to the caller
  ;; that it encountered EOF before the closing delimiter was read,
  ;; i.e. "'foo' bar='baz'" and "'foo" yield the same result, namely
  ;; the values "foo" and #\'
  (handler-case
    (let* ((peek-char (peek-char))
           (delimiter (find peek-char '(#\' #\"))))
      (when delimiter
        (read-char)
        (when write-through
          (write-char delimiter)))
      (multiple-value-prog1
        (values
         (read-while (if delimiter
                       (lambda (c) (char/= c delimiter))
                       (lambda (c) (name-char-p c)))
                     :skip skip
                     :write-through write-through)
         delimiter)
        (when delimiter
          (read-char)
          (when write-through
            (write-char delimiter)))))
    (end-of-file ()
      ;; this can only happen if the very first PEEK-CHAR fails,
      ;; otherwise EOF is handled by READ-WHILE
      nil)))

(declaim (inline read-name))
(defun read-name (&key (skip t) (write-through t))
  "Read characters from *STANDARD-INPUT* as long as they are name
constituents. Returns the string which was read unless SKIP is
true. On EOF the string read so far is returned. Writes all characters
read to *STANDARD-OUTPUT* if WRITE-THROUGH is true."
  (read-while #'name-char-p :skip skip :write-through write-through))

(defun read-attribute (&key (skip t) (write-through t))
  "Read characters from *STANDARD-INPUT* assuming that they constitue
a SGML-style attribute/value pair. Returns three values - the name of
the attribute, its value, and the whole string which was read. On EOF
the string(s) read so far is/are returned. If SKIP is true NIL is
returned. Writes all characters read to *STANDARD-OUTPUT* if
WRITE-THROUGH is true."
  (let* ((name (read-name :skip skip
                          :write-through write-through))
         (whitespace1 (skip-whitespace :skip skip
                                       :write-through write-through)))
    (cond ((eql (peek-char*) #\=)
            (read-char)
            (when write-through
              (write-char #\=))
            (let ((whitespace2 (skip-whitespace :skip skip :write-through write-through)))
              (multiple-value-bind (value delimiter)
                  (read-delimited-string :skip skip :write-through write-through)
                (let ((delimiter-string (if delimiter (string delimiter) "")))
                  (if skip
                    nil
                    (values name value
                            (concatenate 'string
                                         name whitespace1 "=" whitespace2
                                         delimiter-string value delimiter-string)))))))
          (t (if skip
               nil
               (values name nil
                       (concatenate 'string name whitespace1)))))))

(defun skip-comment ()
  "Skip SGML comment from *STANDARD-INPUT*, i.e. a string enclosed in
'--' on both sides. Returns no values. Writes all characters read to
*STANDARD-OUTPUT*. This function assumes \(without checking) that the
current position of *STANDARD-INPUT* is at the beginning of a comment,
after the first hyphen - see COMMENT-START-P."
  (read-char)
  (write-string "--")
  (read-until "--")
  (values))

