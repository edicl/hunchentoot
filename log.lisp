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

(defmacro with-log-file ((stream-var pathname lock) &body body)
  "Helper macro which executes BODY only if PATHNAME \(which is
evaluated) is not NIL.  In this case, the file designated by PATHNAME
is opened for writing \(appending) and created if it doesn't exist.
STREAM-VAR is then bound to a flexi stream which can be used to write
characters to the file in UTF-8 format."
  (with-unique-names (binary-stream)
    (with-rebinding (pathname)
      `(when ,pathname
         (with-lock-held (,lock)
           (with-open-file (,binary-stream ,pathname
                                           :direction :output
                                           :element-type 'octet
                                           :if-does-not-exist :create
                                           :if-exists :append
                                           #+:openmcl #+:openmcl
                                           :sharing :lock)
             (let ((,stream-var (make-flexi-stream ,binary-stream :external-format +utf-8+)))
               ,@body)))))))

(defun log-message-to-file (log-level format-string &rest format-arguments)
  "Sends a formatted message to the file denoted by
*MESSAGE-LOG-PATHNAME* \(unless this value is NIL).  FORMAT and ARGS
are as in FORMAT.  LOG-LEVEL is a keyword denoting the log level or
NIL in which case it is ignored."
  (with-log-file (stream *message-log-pathname* *message-log-lock*)
    (format stream "[~A~@[ [~A]~]] ~?~%"
            (iso-time) log-level
            format-string format-arguments)))

(defun log-access-to-file (&key return-code content content-length)
  "Sends a standardized access log message to the file denoted by
*ACCESS-LOG-FILE* with information about the current request and
response."
  (with-log-file (stream *access-log-pathname* *access-log-lock*)
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

(defun log-message (log-level format-string &rest format-arguments)
  "Convenience function which calls the message logger of the current
acceptor \(if there is one) with the same arguments it accepts.

Returns NIL if there is no message logger or whatever the message
logger returns.

This is the function which Hunchentoot itself uses to log errors it
catches during request processing."
  (when-let (message-logger (acceptor-message-logger *acceptor*))
    (apply message-logger log-level format-string format-arguments)))
  
