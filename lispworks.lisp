;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz. All rights reserved.

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
  ;; make sure socket code is loaded
  (require "comm"))

(defun get-env-variable-as-directory (name)
  "Retrieves the environment variable named NAME and interprets it as
the pathname of a directory which is returned."
  (lw:when-let (string (lw:environment-variable name))
    (when (plusp (length string))
      (cond ((find (char string (1- (length string))) "\\/" :test #'char=) string)
            (t (lw:string-append string "/"))))))

#+(and :lispworks4.4 (or :win32 :linux))
(let ((id :system-cons-free-chain))
  (unless (scm::patch-id-loaded-p id)
    (error "You need a patch to improve the performance of this code. Request patch ~S for ~A for ~A from lisp-support@lispworks.com using the Report Bug command."
          id (lisp-implementation-type)
          #+:win32 "Windows"
          #+:linux "Linux")))

(defvar *cleanup-interval* 100
  "Should be NIL or a positive integer.  The system calls
*CLEANUP-FUNCTION* whenever *CLEANUP-INTERVAL* new worker threads
\(counted globally across all acceptors) have been created unless the
value is NIL.  The initial value is 100.

This variable is only available on LispWorks.")

(defvar *cleanup-function* 'cleanup-function
  "A designator for a function without arguments which is called on a
regular basis if *CLEANUP-INTERVAL* is not NIL.  The initial value is
the name of a function which invokes a garbage collection on 32-bit
versions of LispWorks.

This variable is only available on LispWorks.")

(defvar *worker-counter* 0
  "Internal counter used to count worker threads.  Needed for
*CLEANUP-FUNCTION*.")

(defun cleanup-function ()
  "The default for *CLEANUP-FUNCTION*.  Invokes a GC on 32-bit
LispWorks."
  #-:lispworks-64bit
  (hcl:mark-and-sweep 2))

(defun get-peer-address-and-port (socket)
  "Returns the peer address and port of the socket SOCKET as two
values.  The address is returned as a string in dotted IP address
notation."
  (multiple-value-bind (peer-addr peer-port)
      (comm:get-socket-peer-address socket)
    (values (ignore-errors (comm:ip-address-string peer-addr)) peer-port)))

(defun get-local-address-and-port (socket)
  "Returns the local address and port of the socket SOCKET as two
values.  The address is returned as a string in dotted IP address
notation."
  (multiple-value-bind (local-addr local-port)
      (comm:get-socket-address socket)
    (values (ignore-errors (comm:ip-address-string local-addr)) local-port)))

(eval-when (:compile-toplevel :load-toplevel)
  (when (let ((sym (find-symbol "STREAM-READ-TIMEOUT" :stream)))
          (and sym (fboundp sym)))
    (pushnew :stream-has-timeouts *features*)))

(defun make-socket-stream (socket acceptor)
  "Returns a stream for the socket SOCKET.  The ACCEPTOR argument is
used to set the timeouts."
  #-stream-has-timeouts
  (when (acceptor-write-timeout acceptor)
    (parameter-error "You need LispWorks 5 or higher for write timeouts."))
  (make-instance 'comm:socket-stream
                 :socket socket
                 :direction :io
                 :read-timeout (acceptor-read-timeout acceptor)
                 #+stream-has-timeouts #+stream-has-timeouts
                 :write-timeout (acceptor-write-timeout acceptor)
                 :element-type 'octet))

(defun make-lock (name)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  (mp:make-lock :name name))

(defmacro with-lock-held ((lock) &body body)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  `(mp:with-lock (,lock) ,@body))

;; some help for the IDE
(dspec:define-dspec-alias defvar-unbound (name)
  `(defparameter ,name))

(dspec:define-dspec-alias def-http-return-code (name)
  `(defconstant ,name))

(editor:setup-indent "defvar-unbound" 1 2 4)

(editor:setup-indent "def-http-return-code" 1 2 4)

(editor:setup-indent "handler-case*" 1 2 4)

(defun make-condition-variable (&key name)
  (declare (ignore name))
  (mp:make-condition-variable))

(defun condition-variable-signal (condition-variable)
  (mp:condition-variable-signal condition-variable))

(defun condition-variable-wait (condition-variable lock)
  (mp:condition-variable-wait condition-variable lock))
