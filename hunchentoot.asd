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

(in-package :cl-user)

(defpackage :hunchentoot-asd
  (:use :cl :asdf))

(in-package :hunchentoot-asd)

(defsystem :hunchentoot
  :serial t
  :version "1.3.0"
  :description "Hunchentoot is a HTTP server based on USOCKET and
  BORDEAUX-THREADS.  It supports HTTP 1.1, serves static files, has a
  simple framework for user-defined handlers and can be extended
  through subclassing."
  :license "BSD-2-Clause"
  :depends-on (:chunga
               :cl-base64
               :cl-fad
               :cl-ppcre
               :flexi-streams
               #-(or :lispworks :hunchentoot-no-ssl) :cl+ssl
               :md5
               :alexandria
               :rfc2388
               :trivial-backtrace
               #-:lispworks :usocket
               #-:lispworks :bordeaux-threads)
  :components ((:module url-rewrite
                :serial t
                :components ((:file "packages")
                             (:file "specials")
                             (:file "primitives")
                             (:file "util")
                             (:file "url-rewrite")))
               (:file "packages")
               #+:lispworks (:file "lispworks")
               #-:lispworks (:file "compat")
               (:file "specials")
               (:file "conditions")
               (:file "mime-types")
               (:file "util")
               (:file "log")
               (:file "cookie")
               (:file "reply")
               (:file "request")
               (:file "session")
               (:file "misc")
               (:file "headers")
               (:file "set-timeouts")
               (:file "taskmaster")
               (:file "acceptor")
               #-:hunchentoot-no-ssl (:file "ssl")
               (:file "easy-handlers")))

(defsystem :hunchentoot-test
  :description "Self test functionality for the Hunchentoot HTTP server."
  :components ((:module "test"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test-handlers")
                                     (:file "script-engine")
                                     (:file "script"))))
  :depends-on (:hunchentoot :cl-who :cl-ppcre :drakma))

(defmethod perform ((o test-op) (c (eql (find-system 'hunchentoot))))
  (load (merge-pathnames "run-test.lisp" (system-source-directory c))))

(defsystem :hunchentoot-dev
    :description "Development tools for Hunchentoot development and releases"
    :components ((:file "make-docstrings"))
    :depends-on (:hunchentoot
                 :hunchentoot-test
                 :xpath
                 :cxml-stp
                 :swank))
