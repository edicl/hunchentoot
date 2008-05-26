;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/unix-cmu.lisp,v 1.6 2008/02/13 16:02:19 edi Exp $

;;; Copyright (c) 2004-2008, Dr. Edmund Weitz.  All rights reserved.

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

(defun setuid (uid)
  "Sets the effective user ID of the current process to UID - see
setuid\(2)."
  (multiple-value-bind (return-value errno)
      (unix:unix-setuid uid)
    (unless (and return-value (zerop return-value))
      (error "setuid failed: ~A" (unix:get-unix-error-msg errno)))))

(defun setgid (gid)
  "Sets the effective group ID of the current process to GID -
see setgid\(2)."
  (multiple-value-bind (return-value errno)
      (unix:unix-setgid gid)
    (unless (and return-value (zerop return-value))
      (error "setgid failed: ~A" (unix:get-unix-error-msg errno)))))

(defun get-uid-from-name (name)
  "Returns the UID for the user named NAME."
  (unix:user-info-uid (unix:unix-getpwnam name)))

(defun get-gid-from-name (name)
  "Returns the GID for the group named NAME."
  (unix:group-info-gid (unix:unix-getgrnam name)))
