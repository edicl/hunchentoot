;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

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

(defun set-timeouts (usocket read-timeout write-timeout)
  "Sets up timeouts on the given USOCKET object.  READ-TIMEOUT is the
read timeout period, WRITE-TIMEOUT is the write timeout, specified in
\(fractional) seconds.  The timeouts can either be implemented using
the low-level socket options SO_RCVTIMEO and SO_SNDTIMEO or some
other, implementation specific mechanism.  On platforms that do not
support separate read and write timeouts, both must be equal or an
error will be signaled.  READ-TIMEOUT and WRITE-TIMEOUT may be NIL,
which means that the corresponding socket timeout value will not be
set."
  (declare (ignorable usocket read-timeout write-timeout))
  ;; add other Lisps here if necessary
  #+(or :sbcl :cmu :abcl :mezzano)
  (unless (eql read-timeout write-timeout)
    (parameter-error "Read and write timeouts for socket must be equal."))
  #+:clisp
  (when read-timeout
    (socket:socket-options (usocket:socket usocket) :SO-RCVTIMEO read-timeout))
  #+:clisp
  (when write-timeout
    (socket:socket-options (usocket:socket usocket) :SO-SNDTIMEO write-timeout))
  #+:ecl
  (when read-timeout
    (setf (sb-bsd-sockets:sockopt-receive-timeout (usocket:socket usocket))
	  read-timeout))
  #+:ecl
  (when write-timeout
    (setf (sb-bsd-sockets:sockopt-send-timeout (usocket:socket usocket))
	  write-timeout))
  #+:openmcl
  (when read-timeout
    (setf (ccl:stream-input-timeout (usocket:socket usocket))
          read-timeout))
  #+:openmcl
  (when write-timeout
    (setf (ccl:stream-output-timeout (usocket:socket usocket))
          write-timeout))
  #+:sbcl
  (when read-timeout
    (setf (sb-impl::fd-stream-timeout (usocket:socket-stream usocket))
          (coerce read-timeout 'single-float)))
  #+:cmu
  (setf (lisp::fd-stream-timeout (usocket:socket-stream usocket))
        (coerce read-timeout 'integer))
  #+:abcl
  (when read-timeout
    (java:jcall (java:jmethod "java.net.Socket" "setSoTimeout"  "int")
                (usocket:socket usocket)
                (* 1000 read-timeout)))
  #+:abcl
  (when write-timeout
    (warn "Unimplemented."))
  #+:clasp
  (warn "set-timeouts unimplemented.")
  #+:mezzano
  (let ((connection (mezzano.network.tcp:tcp-stream-connection (usocket:socket usocket))))
    (setf (mezzano.network.tcp:tcp-connection-timeout connection) read-timeout))
  #-(or :clisp :allegro :openmcl :sbcl :lispworks :cmu :ecl :abcl :clasp :mezzano)
  (not-implemented 'set-timeouts))
