;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/unix-lw.lisp,v 1.5 2008/02/13 16:02:19 edi Exp $

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

(fli:define-foreign-function (%setuid "setuid")
    ((uid :int))
  :result-type :int)

(defun setuid (uid)
  "Sets the effective user ID of the current process to UID - see
setuid\(2)."
  (unless (zerop (%setuid uid))
    (parameter-error "setuid failed: ~A" (lw:get-unix-error (lw:errno-value)))))

(fli:define-foreign-function (%setgid "setgid")
    ((gid :int))
  :result-type :int)

(defun setgid (gid)
  "Sets the effective group ID of the current process to GID -
see setgid\(2)."
  (unless (zerop (%setgid gid))
    (parameter-error "setgid failed: ~A" (lw:get-unix-error (lw:errno-value)))))

(fli:define-c-struct passwd
  (name (:pointer :char))
  (passwd (:pointer :char))
  (uid :int)
  (gid :int)
  (gecos (:pointer :char))
  (dir (:pointer :char))
  (shell (:pointer :char)))

(fli:define-foreign-function (getpwnam "getpwnam")
    ((name (:reference-pass :ef-mb-string)))
  :result-type (:pointer passwd))

(defun get-uid-from-name (name)
  "Returns the UID for the user named NAME."
  (let ((passwd (getpwnam name)))
    (when (fli:null-pointer-p passwd)
      (let ((errno (lw:errno-value)))
        (cond ((zerop errno)
               (parameter-error "User ~S not found." name))
              (t (parameter-error "getpwnam failed: ~A" (lw:get-unix-error errno))))))
    (fli:foreign-slot-value passwd 'uid)))

(fli:define-c-struct group
  (name (:pointer :char))
  (passwd (:pointer :char))
  (gid :int)
  (mem (:pointer (:pointer :char))))

(fli:define-foreign-function (getgrnam "getgrnam")
    ((name (:reference-pass :ef-mb-string)))
  :result-type (:pointer group))

(defun get-gid-from-name (name)
  "Returns the GID for the group named NAME."
  (let ((group (getgrnam name)))
    (when (fli:null-pointer-p group)
      (let ((errno (lw:errno-value)))
        (cond ((zerop errno)
               (parameter-error "Group ~S not found." name))
              (t (parameter-error "getgrnam failed: ~A" (lw:get-unix-error errno))))))
    (fli:foreign-slot-value group 'gid)))