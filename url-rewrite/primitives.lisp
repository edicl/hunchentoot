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

(declaim (inline peek-char*))
(defun peek-char* ()
  "PEEK-CHAR with input stream bound to *STANDARD-INPUT* and returning
NIL on EOF."
  (peek-char nil nil nil))

(declaim (inline whitespacep))
(defun whitespacep (c)
  "Checks whether C is a whitespace character."
  (find c '(#\Space #\Tab #\Newline #\Linefeed #\Return #\Page)))

(declaim (inline letterp))
(defun letterp (c)
  "Checks whether C is a character between A and Z
\(case-insensitive)."
  (and (characterp c)
       (or (char<= #\a c #\z)
           (char<= #\A c #\Z))))

(declaim (inline name-char-p))
(defun name-char-p (c)
  "Checks whether C is a name constituent character in the sense of
HTML."
  (and (characterp c)
       (or (letterp c)
           (digit-char-p c)
           (char= c #\-)
           (char= c #\.))))

(defun comment-start-p ()
  "Checks whether *STANDARD-OUTPUT* currently 'looks at' the string
\"--\".  Will move the position within the stream by one unless the
first characters it sees is not a hyphen."
  (unless (eql (peek-char*) #\-)
    ;; if the first character isn't #\- we can return immediately
    (return-from comment-start-p nil))
  ;; otherwise read the #\- so we can check the next character
  (read-char)
  (eql (peek-char*) #\-))

(defun read-while (predicate &key (skip t) (write-through t))
  "Reads characters from *STANDARD-INPUT* while PREDICATE returns a
true value for each character.  Returns the string which was read
unless SKIP is true.  Writes all characters read to *STANDARD-OUTPUT*
if WRITE-THROUGH is true.  On EOF the string read so far is returned."
  (let ((collector (or skip
                       (make-array 0
                                   :element-type 'character
                                   :fill-pointer t
                                   :adjustable t))))
    (handler-case
      (loop while (funcall predicate (peek-char)) do
            (let ((char (read-char)))
              (when write-through
                (write-char char))
              (unless skip
                (vector-push-extend char collector)))
            finally (return (and (not skip) collector)))
      (end-of-file ()
        (and (not skip) collector)))))

(defun read-until (string &key (skip t) (write-through t))
  "Reads characters from *STANDARD-INPUT* up to and including STRING.
Returns the string which was read \(excluding STRING) unless SKIP is
true.  Writes all characters read to *STANDARD-OUTPUT* if
WRITE-THROUGH is true.  On EOF the string read so far is returned."
  (let* ((length (length string))
         (offsets
           ;; we first check whether some substring which starts
           ;; STRING can be found again later in STRING - this is
           ;; necessary because we only peek one character ahead
           (cond ((gethash string *find-string-hash*))
                 (t (setf (gethash string *find-string-hash*)
                            ;; the resulting array of offsets is
                            ;; cached in *FIND-STRING-HASH* so we can
                            ;; use it again in case READ-UNTIL is
                            ;; called with the same STRING argument
                            (loop with offsets = (make-array length
                                                             :initial-element nil)
                                  for i from 1 below length
                                  ;; check if STRING starting from 0
                                  ;; has something in common with
                                  ;; STRING starting from I
                                  for mismatch = (mismatch string string
                                                           :start1 i :test #'char=)
                                  when (> mismatch i)
                                  ;; if this is the case remember the
                                  ;; length of the match plus the
                                  ;; character which must follow in
                                  ;; OFFSETS
                                  do (push (cons (char string (- mismatch i))
                                                 (1+ (- mismatch i)))
                                           (svref offsets i))
                                  finally (return offsets))))))
         (collector (or skip
                        (make-array 0
                                    :element-type 'character
                                    :fill-pointer t
                                    :adjustable t))))
    (handler-case
      (loop for i = 0 then (cond (match (1+ i))
                                 ;; if there is an offset (see above)
                                 ;; we don't have to start from the
                                 ;; beginning of STRING
                                 ((cdr (assoc c (svref offsets i))))
                                 (t 0))
            for c = (peek-char)
            for match = (char= c (char string i))
            while (or (not match) (< (1+ i) length)) do
            (cond (skip (read-char))
                  (t (vector-push-extend (read-char) collector)))
            when write-through do
            (write-char c)
            finally (if write-through
                      (write-char (read-char))
                      (read-char))
            (unless skip
              ;; decrement the fill pointer because collector now also
              ;; contains STRING itself
              (decf (fill-pointer collector) (1- length)))
            (return (and (not skip) collector)))
      (end-of-file ()
        (and (not skip) collector)))))

