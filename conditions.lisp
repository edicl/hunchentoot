;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-

;;; Copyright (c) 2008-2009, Dr. Edmund Weitz.  All rights reserved.

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

(define-condition hunchentoot-condition (condition)
  ()
  (:documentation "Superclass for all conditions related to Hunchentoot."))

(define-condition hunchentoot-error (hunchentoot-condition error)
  ()
  (:documentation "Superclass for all errors related to Hunchentoot."))

(define-condition hunchentoot-simple-error (hunchentoot-error simple-condition)
  ()
  (:documentation "Like HUNCHENTOOT-ERROR but with formatting capabilities."))

(defun hunchentoot-error (format-control &rest format-arguments)
  "Signals an error of type HUNCHENTOOT-SIMPLE-ERROR with the provided
format control and arguments."
  (error 'hunchentoot-simple-error
         :format-control format-control
         :format-arguments format-arguments))

(define-condition hunchentoot-warning (hunchentoot-condition warning)
  ()
  (:documentation "Superclass for all warnings related to Hunchentoot."))

(define-condition hunchentoot-simple-warning (hunchentoot-warning simple-condition)
  ()
  (:documentation "Like HUNCHENTOOT-WARNING but with formatting capabilities."))

(defun hunchentoot-warn (format-control &rest format-arguments)
  "Signals a warning of type HUNCHENTOOT-SIMPLE-WARNING with the
provided format control and arguments."
  (warn 'hunchentoot-simple-warning
        :format-control format-control
        :format-arguments format-arguments))

(define-condition parameter-error (hunchentoot-simple-error)
  ()
  (:documentation "Signalled if a function was called with incosistent or illegal parameters."))

(defun parameter-error (format-control &rest format-arguments)
  "Signals an error of type PARAMETER-ERROR with the provided
format control and arguments."
  (error 'parameter-error
         :format-control format-control
         :format-arguments format-arguments))

(define-condition operation-not-implemented (hunchentoot-error)
  ((operation :initarg :operation
              :reader hunchentoot-operation-not-implemented-operation
              :documentation "The name of the unimplemented operation."))
  (:report (lambda (condition stream)
             (format stream "The operation ~A is not yet implemented for the implementation ~A.
Consider sending a patch..."
                     (hunchentoot-operation-not-implemented-operation condition)
                     (lisp-implementation-type))))
  (:documentation "This warning is signalled when an operation \(like
SETUID for example) is not implemented for a specific Lisp."))

(defun not-implemented (name)
  "Used to signal an error if an operation named NAME is not implemented."
  (error 'operation-not-implemented :operation name))

(define-condition bad-request (hunchentoot-error)
  ())

;;;

(defgeneric maybe-invoke-debugger (condition)
  (:documentation "This generic function is called whenever a
condition CONDITION is signaled in Hunchentoot.  You might want to
specialize it on specific condition classes for debugging purposes.")
  (:method (condition)
   "The default method invokes the debugger with CONDITION if
*CATCH-ERRORS-P* is NIL."
   (unless *catch-errors-p*
     (invoke-debugger condition))))

(defmacro with-debugger (&body body)
  "Executes BODY and invokes the debugger if an error is signaled and
*CATCH-ERRORS-P* is NIL."
  `(handler-bind ((bad-request (lambda (c)
                                 (declare (ignore c))
                                 (setf (return-code *reply*) +http-bad-request+)
                                 (abort-request-handler)))
                  (error #'maybe-invoke-debugger))
     ,@body))

(defmacro ignore-errors* (&body body)
  "Like IGNORE-ERRORS, but observes *CATCH-ERRORS-P*."
  `(ignore-errors (with-debugger ,@body)))
       
(defmacro handler-case* (expression &rest clauses)
  "Like HANDLER-CASE, but observes *CATCH-ERRORS-P*."
  `(handler-case (with-debugger ,expression)
     ,@clauses))

(defun get-backtrace ()
  "Returns a string with a backtrace of what the Lisp system thinks is
the \"current\" error."
  (handler-case
      (with-output-to-string (s)
        (trivial-backtrace:print-backtrace-to-stream s))
    (error (condition)
      (format nil "Could not generate backtrace: ~A." condition))))
