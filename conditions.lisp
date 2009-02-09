;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/odd-streams/conditions.lisp,v 1.5 2007/12/31 01:08:45 edi Exp $

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