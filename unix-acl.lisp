;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/unix-acl.lisp,v 1.6 2008/02/13 16:02:19 edi Exp $

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "osi"))

(defun setuid (uid)
  "Sets the effective user ID of the current process to UID - see
setuid\(2)."
  (excl.osi:setuid uid))

(defun setgid (gid)
  "Sets the effective group ID of the current process to GID -
see setgid\(2)."
  (excl.osi:setgid gid))

(defun get-uid-from-name (name)
  "Returns the UID for the user named NAME."
  (excl.osi:pwent-uid  (or (excl.osi:getpwnam name)
                           (parameter-error "User ~S not found." name))))

(defun get-gid-from-name (name)
  "Returns the GID for the group named NAME."
  (excl.osi:grent-gid (or (excl.osi:getgrnam name)
                          (parameter-error "Group ~S not found." name))))
