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

(defun starts-with-scheme-p (string)
  "Checks whether the string STRING represents a URL which starts with
a scheme, i.e. something like 'https://' or 'mailto:'."
  (loop with scheme-char-seen-p = nil
        for c across string
        when (or (char-not-greaterp #\a c #\z)
                 (digit-char-p c)
                 (member c '(#\+ #\- #\.) :test #'char=))
        do (setq scheme-char-seen-p t)
        else return (and scheme-char-seen-p
                         (char= c #\:))))

(defun url-encode (string)
  "URL-encode a string."
  (with-output-to-string (s)
    (loop for c across string
          do (cond ((or (char<= #\0 c #\9)
                        (char<= #\a c #\z)
                        (char<= #\A c #\Z)
                        (find c "$-_.!*'()," :test #'char=))
                     (write-char c s))
                   ((char= c #\Space)
                     (write-char #\+ s))
                   (t (format s "%~2,'0x" (char-code c)))))))

(defun add-get-param-to-url (url name value)
  "URL is assumed to be a http URL. The pair of NAME and VALUE will be
added as a GET parameter to this URL. Assumes that there's no other
parameter of the same name. Only checks if #\? is part of the string
to decide how to attach the new parameter to the end of the string."
  ;; possible bug: doesn't check for #\? which is written as, say,
  ;; "&x3f;" - also, is there any other way a question mark could be a
  ;; legitimate part of a URL?
  (concatenate 'string
               url
               (if (find #\? url :test #'char=)
                 "&amp;"
                 "?")
               name
               "="
               (url-encode value)))

(defun rewrite-urls (rewrite-fn &optional (test-fn (complement #'starts-with-scheme-p)))
  "Reads an \(X)HTML document from *STANDARD-INPUT* and writes it back
to *STANDARD-OUTPUT*. Any attribute value which is in one of the
positions denoted by *URL-REWRITE-TAGS* is rewritten by REWRITE-FN if
it passes the test denoted by the optional function TEST-FN which
defaults to the complement of STARTS-WITH-SCHEME-P.

This function aims to yield correct results for correct \(X)HTML input
and it also tries hard to never signal an error although it may warn
if it encounters syntax errors. It will NOT detect any possible error
nor is there any warranty that it will work correctly with faulty
input."
  (loop
    ;; read (and write back) until we see a #\< which is a candidate
    ;; for a tag or a markup declaration
    (read-until "<")
    ;; get next char without reading it
    (let ((peek-char (peek-char*)))
      (cond ((null peek-char)
              ;; stop if EOF
              (return-from rewrite-urls))
            ((char= peek-char #\!)
              ;; we've seen "<!" so this might be a markup declaration
              ;; - first write #\! back
              (write-char (read-char))
              ;; peek at next char
              (let ((peek-char (peek-char*)))
                (cond ((null peek-char)
                        ;; stop if EOF
                        (return-from rewrite-urls))
                      ((eql peek-char #\>)
                        ;; "<!>" is nothing special, just write the
                        ;; char and go back to the start of the loop
                        (write-char (read-char)))
                      ((letterp peek-char)
                        ;; a letter, so this should be something like
                        ;; <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2
                        ;; Final//EN"> - we just check for names and
                        ;; delimited strings separated by whitespace
                        ;; until we see the next #\>
                        (read-name)
                        (skip-whitespace)
                        (block parameter-loop
                          (loop
                            (let ((peek-char (peek-char*)))
                              (cond ((null peek-char)
                                      ;; stop if EOF
                                      (warn "EOF in markup declaration")
                                      (return-from rewrite-urls))
                                    ((char= peek-char #\>)
                                      ;; a #\> ends the markup
                                      ;; declaration - write it back
                                      ;; and exit the loop
                                      (write-char (read-char))
                                      (return-from parameter-loop))
                                    ((or (letterp peek-char)
                                         (find peek-char '(#\' #\") :test #'char=))
                                      ;; a delimiter or a letter, so
                                      ;; we expect a delimited string
                                      (read-delimited-string)
                                      (skip-whitespace))
                                    ((comment-start-p)
                                      ;; a comment - skip it and write it back
                                      (skip-comment))
                                    (t
                                      ;; something else - this is an error
                                      ;; so we warn and exit the loop
                                      (warn "Unexpected character ~S in markup declaration"
                                            peek-char)
                                      (return-from parameter-loop)))))))
                      ((comment-start-p)
                        ;; we've seen "<!--" so this starts a comment declaration
                        ;; - we'll read comments which are possibly separated
                        ;; by whitespace
                        (block comment-loop
                          (loop
                            (skip-comment)
                            (skip-whitespace)
                            (let ((peek-char (peek-char*)))
                              (cond ((null peek-char)
                                      ;; stop if EOF
                                      (warn "EOF in comment declaration")
                                      (return-from rewrite-urls))
                                    ((char= peek-char #\>)
                                      ;; a #\> ends the comment
                                      ;; declaration - write it back
                                      ;; and exit the loop
                                      (write-char (read-char))
                                      (return-from comment-loop))
                                    ;; a comment - do nothing
                                    ((comment-start-p))
                                    (t
                                      ;; something else - this is an error
                                      ;; so we warn and exit the loop
                                      (warn "Unexpected character ~S in comment declaration"
                                            peek-char)
                                      (return-from comment-loop)))))))
                      (t
                        ;; neither markup declaration nor comment declaration,
                        ;; so this was just "<!"
                        (write-char (read-char))))))
            ((char= peek-char #\/)
              (write-char (read-char))
              (let ((peek-char (peek-char*)))
                (cond ((null peek-char)
                        ;; stop if EOF
                        (warn "EOF in end-tag")
                        (return-from rewrite-urls))
                      ((letterp peek-char)
                        ;; a letter, so this is supposed to start a name -
                        ;; read it and skip whitespace following it
                        (let ((name (read-name :skip nil)))
                          (skip-whitespace)
                          (let ((peek-char (peek-char*)))
                            (cond ((null peek-char)
                                    ;; stop if EOF
                                    (warn "EOF after </~A" name)
                                    (return-from rewrite-urls))
                                  ((char/= (peek-char*) #\>)
                                    ;; we expect to see #\> here - if not
                                    ;; we warn but do nothing else
                                    (warn "Expected #\> after </~A" name))
                                  (t
                                    ;; end of end tag, just consume the #\>
                                    (write-char (read-char)))))))
                      (t
                        ;; not a letter, so this is an error -
                        ;; we warn and ignore this
                        (warn "Unexpected character ~S after </"
                              peek-char)))))
            ((letterp peek-char)
              ;; a letter so we expect a start tag, possibly followed by
              ;; attributes - first read name, check if it's mentioned
              ;; in *URL-REWRITE-TAGS*, and find the name of the
              ;; corresponding attribute
              (let* ((name (read-name :skip nil))
                     (rewrite-attribute (and name
                                             (cdr (assoc name *url-rewrite-tags*
                                                         :test #'string-equal))))
		     attribute-found-p)
                (flet ((maybe-write-attribute (&optional value
                                                         (rewrite-attribute
                                                          (and (not attribute-found-p)
                                                               (cdr (assoc name
                                                                           *url-rewrite-fill-tags*
                                                                           :test #'string-equal)))))
                         ;; write the name of the attribute
                         ;; REWRITE-ATTRIBUTE and its (rewritten)
                         ;; value VALUE to *STANDARD-OUTPUT* if DO-IT
                         ;; is true - the default value for DO-IT
                         ;; means to only write the attribute if it
                         ;; has to be added
                         (when rewrite-attribute
                           (unless attribute-found-p
                             (write-char #\Space))
                           (write-string rewrite-attribute)
                           (write-char #\=)
                           (let ((delimiter (if (find #\' value :test #'char=)
                                              #\" #\')))
                             (write-char delimiter)
                             (write-string (funcall rewrite-fn value))
                             (write-char delimiter)))))
                  (skip-whitespace)
                  (block attribute-loop
                    (loop
                      (let ((peek-char (peek-char*)))
                        (cond ((null peek-char)
                                ;; stop if EOF
                                (warn "EOF before ~A tag was closed" name)
                                (return-from rewrite-urls))
                              ((eql peek-char #\>)
                                ;; end of tag - exit attribute loop
                                (maybe-write-attribute)
                                (write-char (read-char))
                                (return-from attribute-loop))
                              ((eql peek-char #\/)
                                ;; we've seen #\/, so this might be the XHTML way
                                ;; to end a stand-alone tag
                                (write-char (read-char))
                                (cond ((eql (peek-char*) #\>)
                                        ;; yes, it is - exit this loop
                                        (maybe-write-attribute)
                                        (write-char (read-char)))
                                      (t
                                        ;; no, it's not - so this is an error
                                        (warn "Unexpected #\/ in ~A tag" name)))
                                ;; exit attribute loop in any case
                                (return-from attribute-loop))
                              ((letterp peek-char)
                                ;; a letter - this should start an attribute
                                (multiple-value-bind (name value string)
                                    ;; no need to cons up return values if we're
                                    ;; not going to rewrite anyway
                                    (read-attribute :skip (null rewrite-attribute)
                                                    :write-through (null rewrite-attribute))
                                  (cond ((and rewrite-attribute
                                              (string-equal name rewrite-attribute))
                                          ;; remember that we've seen the
                                          ;; attribute in question
                                          (setq attribute-found-p t)
                                          ;; if this an attribute which should be
                                          ;; rewritten do it and write the whole
                                          ;; stuff to *STANDARD-OUT* explicitly
                                          (cond ((funcall test-fn value)
                                                  (maybe-write-attribute value name))
                                                (t
                                                  ;; otherwise write it back
                                                  (write-string string))))
                                        (rewrite-attribute
                                          ;; we didn't rewrite this attribute but we
                                          ;; have to write it back to *STANDARD-OUTPUT*
                                          ;; because READ-ATTRIBUTE didn't do it
                                          (write-string string))))
                                (skip-whitespace))
                              (t
                                ;; an error - exit the attribute loop
                                (warn "Unexpected character ~A after <~A" peek-char name)
                                (return-from attribute-loop)))))))))
            (t
              ;; anything else means this is just #\<, no markup
              (write-char (read-char)))))))
