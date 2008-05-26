;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header$

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

(defclass ssl-server (server)
  ((ssl-certificate-file :initarg :ssl-certificate-file
                         :reader server-ssl-certificate-file
                         :documentation "The namestring of a
certificate file.")
   (ssl-privatekey-file :initarg :ssl-privatekey-file
                        :reader server-ssl-privatekey-file
                        :documentation "The namestring of a private
key file, or NIL if the certificate file contains the private key.")
   (ssl-privatekey-password #+:lispworks #+:lispworks
                            :initform nil
                            :initarg :ssl-privatekey-password
                            :reader server-ssl-privatekey-password
                            :documentation "The password for the
private key file or NIL."))
  (:default-initargs :port 443 :output-chunking-p nil)
  (:documentation "This class defines additional slots required to
serve requests by SSL"))

(defmethod initialize-instance :around ((server ssl-server)
                                        &rest args
                                        &key ssl-certificate-file ssl-privatekey-file
                                        &allow-other-keys)
  (apply #'call-next-method server
         :ssl-certificate-file (namestring ssl-certificate-file)
         :ssl-privatekey-file (namestring (or ssl-privatekey-file
                                              #+:lispworks
                                              ssl-certificate-file))
         args))

#+lispworks
(defun make-ssl-server-stream (socket-stream &key certificate-file privatekey-file privatekey-password)
  "Given the server socket stream SOCKET-STREAM attaches SSL to the
stream using the certificate file CERTIFICATE-FILE and the private key
file PRIVATEKEY-FILE.  Both of these values must be namestrings
denoting the location of the files.  If PRIVATEKEY-PASSWORD is not NIL
then it should be the password for the private key file \(if
necessary).  Returns the stream"
  (flet ((ctx-configure-callback (ctx)
           (when privatekey-password
             (comm:set-ssl-ctx-password-callback ctx :password privatekey-password))
           (comm:ssl-ctx-use-certificate-file ctx
                                              certificate-file
                                              comm:ssl_filetype_pem)
           (comm:ssl-ctx-use-privatekey-file ctx
                                             privatekey-file
                                             comm:ssl_filetype_pem)))
    (comm:attach-ssl socket-stream
                     :ctx-configure-callback #'ctx-configure-callback)
    socket-stream))


(defmethod server-ssl-p ((server ssl-server))
  t)

(defmethod initialize-connection-stream ((server ssl-server) stream)
  ;; attach SSL to the stream if necessary
  (call-next-method server
                    #+:lispworks
                    (make-ssl-server-stream stream
                                            :certificate-file (server-ssl-certificate-file server)
                                            :privatekey-file (server-ssl-privatekey-file server)
                                            :privatekey-password (server-ssl-privatekey-password server))
                    #-:lispworks
                    (cl+ssl:make-ssl-server-stream stream
                                                   :certificate (server-ssl-certificate-file server)
                                                   :key (server-ssl-privatekey-file server))))