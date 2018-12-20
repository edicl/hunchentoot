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

(in-package :hunchentoot)

(defmacro with-log-stream ((stream-var destination lock) &body body)
  "Helper macro to write log entries.  STREAM-VAR is a symbol that
will be bound to the logging stream during the execution of BODY.
DESTINATION is the logging destination, which can be either a pathname
designator of the log file, a symbol designating an open stream or NIL
if no logging should be done.  LOCK refers to the lock that should be
held during the logging operation.  If DESTINATION is a pathname, a
flexi stream with UTF-8 encoding will be created and bound to
STREAM-VAR.  If an error occurs while writing to the log file, that
error will be logged to *ERROR-OUTPUT*.

Note that logging to a file involves opening and closing the log file
for every logging operation, which is overall costly.  Web servers
with high throughput demands should make use of a specialized logging
function rather than relying on Hunchentoot's default logging
facility."
  (with-gensyms (binary-stream)
    (once-only (destination)
      (let ((body body))
        `(when ,destination
           (with-lock-held (,lock)
             (etypecase ,destination
               ((or string pathname)
                (with-open-file (,binary-stream ,destination
                                                :direction :output
                                                :element-type 'octet
                                                :if-does-not-exist :create
                                                :if-exists :append
                                                #+:openmcl :sharing #+:openmcl :lock)
                  (let ((,stream-var (make-flexi-stream ,binary-stream :external-format +utf-8+)))
                    ,@body)))
               (stream
                (let ((,stream-var ,destination))
                  (prog1 (progn ,@body)
                    (finish-output ,destination)))))))))))
  
