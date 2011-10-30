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

(in-package :hunchentoot-test)

(defun file-contents (pathname &key (element-type '(unsigned-byte 8)))
  (with-open-file (s pathname :element-type element-type)
    (let ((result (make-array (file-length s) :element-type element-type)))
      (read-sequence result s)
      result)))  

(defclass script-context ()
  ((base-url :initarg :base-url
             :reader script-context-base-url
             :documentation "Base URL to be used for all HTTP requests in this script context")))

(defmethod initialize-instance :before ((script-context script-context) &key context-class-name)
  ;; just ignore the context-class-name so that we can use &args in the WITH-SCRIPT-CONTEXT macro below.
  (declare (ignore context-class-name)))

(defvar *script-context* nil
  "Current script context")

(defmacro with-script-context ((&rest args &key (context-class-name 'script-context) &allow-other-keys)
                               &body body)
  `(let ((*script-context* (make-instance ',context-class-name ,@args))
         (*default-pathname-defaults* *this-file*)
         failed)
     (handler-bind
        ((assertion-failed (lambda (condition)
                             (push condition failed)
                             (format t "Assertion failed:~%~A~%" condition))))
       (prog1
           (progn ,@body
                  (values))
         (if failed
             (format t ";; ~A assertion~:P FAILED~%" (length failed))
             (format t ";; all tests PASSED~%"))))))

(defclass http-reply ()
  ((body :initarg :body)
   (status-code :initarg :status-code)
   (headers :initarg :headers)
   (uri :initarg :uri)
   (stream :initarg :stream)
   (close :initarg :close)
   (reason-phrase :initarg :reason-phrase)))

(defvar *last-reply* nil
  "Contains the last HTTP reply received")

(define-condition assertion-failed (simple-condition)
  ((assertion :initarg :assertion
              :accessor condition-assertion
              :initform nil)
   (reply-slot-name :initarg :reply-slot-name
                    :reader condition-reply-slot-name)
   (reply-value :initarg :reply-value
                :reader condition-reply-value)
   (operator :initarg :operator
             :reader condition-operator)
   (args :initarg :args
         :reader condition-args)
   (reply :initarg :reply
          :reader condition-reply))
  (:report print-assertion))

(defun print-assertion (condition stream)
  (format stream " (~A "
          (condition-operator condition))
  (loop
     for rest on (cons (condition-reply-value condition)
                       (condition-args condition))
     for value = (car rest)
     for more-p = (cdr rest)
     do (if (and (arrayp value) (not (stringp value)))
            (format stream "<array>")
            (format stream "~S" value))
     when more-p
       do (princ #\Space stream))
  (format stream ")~%"))

(defun function-designator-p (thing)
  "Return true value if THING is a function or a symbol that has a function definition."
  (or (functionp thing)
      (and (symbolp thing)
           (fboundp thing))))

(defmacro with-operator-defaulting ((default-operator) &body body)
  "If OPERATOR is not a function designator, prepend it to ARGS and
bind OPERATOR to DEFAULT-OPERATOR.  OPERATOR and ARGS are captured
from the expansion environment."
  `(if (function-designator-p operator)
       (progn ,@body)
       (let ((operator ',default-operator)
             (args (cons operator args)))
         ,@body)))

(defun http-assert (reply-slot-name operator &rest args)
  (let ((reply-value (slot-value *last-reply* reply-slot-name)))
    (with-operator-defaulting (equal)
      (unless (apply operator reply-value args)
        (signal 'assertion-failed
                :reply-slot-name reply-slot-name
                :reply-value reply-value
                :operator operator
                :args args
                :reply *last-reply*)))))

(define-condition header-assertion-failed (assertion-failed)
  ((header-name :initarg :header-name :reader condition-header-name)))

(defun http-assert-header (header-name operator &rest args)
  (let ((header-value (cdr (assoc header-name (slot-value *last-reply* 'headers) :test #'string-equal))))
    (with-operator-defaulting (matches)
      (unless (apply operator header-value args)
        (signal 'header-assertion-failed
                :reply-slot-name 'headers
                :header-name header-name
                :reply-value header-value
                :operator operator
                :args args
                :reply *last-reply*)))))

(defun http-assert-body (regex)
  (http-assert 'body 'matches regex))

(defun matches (string regex)
  (cl-ppcre:scan regex string))

(defun integer-equal (string integer)
  (eql (parse-integer string) integer))

(defun http-request (url
                     &rest args
                     &key (protocol :http/1.1)
                          (method :get)
                          content
                          content-type
                          content-length
                          range
                          cookie-jar
                          basic-authorization
                          parameters
                          external-format-out
                          additional-headers)
  (declare (ignore protocol method content content-type content-length cookie-jar basic-authorization
                   range parameters external-format-out additional-headers))
  (setf *last-reply* (make-instance 'http-reply))
  (with-slots (body status-code headers uri stream close) *last-reply*
    (setf (values body status-code headers uri stream close)
          (apply 'drakma:http-request
                 (format nil "~A~A" (script-context-base-url *script-context*) url)
                 args)))
  (values))
