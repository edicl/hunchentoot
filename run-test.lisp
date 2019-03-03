;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-

;;; Copyright (c) 2011, Hans Huebner.  All rights reserved.

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

(in-package :cl-user)

(defparameter *test-port* 4241)

(asdf:oos 'asdf:load-op :hunchentoot-test)

(defun run-tests ()
  (format t "~&;; Starting web server on localhost:~A." *test-port*)
  (force-output)
  (let ((server (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port *test-port*))))
    (unwind-protect
         (progn
           (format t "~&;; Sleeping 2 seconds to give the server some time to start...")
           (force-output)
           (sleep 2)
           (format t "~&;; Now running confidence tests.")
           (force-output)
           (hunchentoot-test:test-hunchentoot (format nil "http://localhost:~A" *test-port*)))
      (format t "~&;; Stopping server.")
      (force-output)
      (hunchentoot:stop server)
      (format t "~&;; Cleaning temporary files.")
      (hunchentoot-test::clean-tmp-dir))))

(run-tests)
