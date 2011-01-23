;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/log.lisp,v 1.10 2008/02/13 16:02:17 edi Exp $

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

(in-package :hunchentoot)

(defmacro with-open-file-or-console ((stream-var pathname lock) &body body)
  "Helper macro to write log entries.  STREAM-VAR is a symbol that
will be bound to the logging stream during the execution of BODY.
PATHNAME is the pathname designator of the log file or NIL if logging
should be done to *ERROR-OUTPUT*.  LOCK refers to the lock that should
be held during the logging operation.  If PATHNAME is not NIL, a flexi
stream with UTF-8 encoding will be created and bound to STREAM-VAR.
If an error occurs while writing to the log file, that error will be
logged to *ERROR-OUTPUT*."
  (with-unique-names (binary-stream)
    (with-rebinding (pathname)
      (let ((body body))
        `(if ,pathname
             (with-lock-held (,lock)
               (with-open-file (,binary-stream ,pathname
                                               :direction :output
                                               :element-type 'octet
                                               :if-does-not-exist :create
                                               :if-exists :append
                                               #+:openmcl #+:openmcl
                                               :sharing :lock)
                 (let ((,stream-var (make-flexi-stream ,binary-stream :external-format +utf-8+)))
                   ,@body)))
             (let ((,stream-var *error-output*))
               (prog1 (progn ,@body)
                 (finish-output *error-output*))))))))
  
