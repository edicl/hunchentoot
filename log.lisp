;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/log.lisp,v 1.10 2008/02/13 16:02:17 edi Exp $

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

(defclass log-file ()
  ((pathname :initarg :pathname
             :accessor log-file-pathname
             :documentation "Current pathname of this LOG-FILE
instance.")
   (stream :initform nil
           :accessor log-file-stream
           :documentation "Open stream to current log file.")
   (lock :initform (make-lock "Hunchentoot log file lock")
         :reader log-file-lock
         :documentation "Lock to guard write access to the log file.
Every write to the log file is done with the lock held to prevent
unwanted mixing of log entries.")))

(defun ensure-log-stream-open (log-file)
  "Ensures that the stream to the given LOG-FILE instance is open."
  (unless (log-file-stream log-file)
    (setf (log-file-stream log-file)
          (make-flexi-stream (open (ensure-directories-exist (log-file-pathname log-file))
                                   :direction :output
                                   :element-type 'octet
                                   :if-does-not-exist :create
                                   :if-exists :append
                                   #+:openmcl #+:openmcl
                                   :sharing :lock)
                             :external-format +utf-8+))))
    
(defmacro with-log-file ((stream-var log-file) &body body)
  "Execute BODY with STREAM-VAR bound to the stream of the LOG-FILE,
which is assumed to be an instance of the LOG-FILE class.  The stream
will be opened if neccessary, and the lock of the LOG-FILE will be
held during execution of BODY."
  (with-unique-names (retval)
    `(with-lock-held ((log-file-lock ,log-file))
       (ensure-log-stream-open ,log-file)
       ;; In order to reduce logging overhead, we print the message
       ;; to to an in-memory buffer and then send it to the
       ;; FLEXI-STREAM in one operation in the hope to reduce
       ;; external format conversion overhead.
       (let* ((,stream-var (make-string-output-stream))
              (,retval (multiple-value-list (progn ,@body))))
         (princ (get-output-stream-string ,stream-var)
                (log-file-stream ,log-file))
         (force-output (log-file-stream ,log-file))
         (values-list ,retval)))))

(defmacro define-log-file (visible-name special-variable pathname documentation)
  "Define a log file with VISIBLE-NAME being the name of the getter
and setter function to change the path of the log file,
SPECIAL-VARIABLE being the special variable bound to the LOG-FILE
instance that represents the log file internally, PATHNAME be the
default pathname to the log file and DOCUMENTATION be the docstring to
be used for all three of them."
  `(progn
     
     (defvar ,special-variable (make-instance 'log-file :pathname ,pathname)
       ,(format nil "The ~A" documentation))

     (defun ,visible-name ()
       ,(format nil "Returns the ~A" documentation)
       (log-file-pathname ,special-variable))

     (defun (setf ,visible-name) (pathname)
       ,(format nil "Sets the ~A" documentation)
       (with-lock-held ((log-file-lock ,special-variable))
         (when (log-file-stream ,special-variable)
           (close (log-file-stream ,special-variable))
           (setf (log-file-stream ,special-variable) nil))
         (setf (log-file-pathname ,special-variable) pathname)))))

(define-log-file log-file *log-file* *log-pathname*
  "File to use to log general messages.")

(defmethod log-message (log-level format &rest args)
  "Sends a formatted message to the file denoted by *LOG-FILE*.
FORMAT and ARGS are as in FORMAT.  LOG-LEVEL is a keyword denoting the
log level or NIL in which case it is ignored."
  (with-log-file (stream *log-file*)
    (format stream "[~A~@[ [~A]~]] " (iso-time) log-level)
    (apply #'format stream format args)
    (terpri stream)))

(defun log-message* (log-level format &rest args)
  "Internal function accepting the same arguments as LOG-MESSAGE and
using the message logger of *ACCEPTOR* \(if there is one)."
  (when-let (message-logger (acceptor-message-logger *acceptor*))
    (apply message-logger log-level format args)))

(define-log-file access-log-file *access-log-file* *access-log-pathname*
  "File to use to log access messages.")

(defun log-access (&key return-code content content-length)
  "Sends a standardized access log message to the file denoted by
*ACCESS-LOG-FILE* with information about the current request and
response."
  (with-log-file (stream *access-log-file*)
    (format stream "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] [~A] \"~A ~A~@[?~A~] ~
                    ~A\" ~A ~:[~*-~;~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\"~%"
            (remote-addr*)
            (header-in* :x-forwarded-for)
            (authorization)
            (iso-time)
            (request-method*)
            (script-name*)
            (query-string*)
            (server-protocol*)
            return-code
            content
            content-length
            (referer)
            (user-agent))))
  
