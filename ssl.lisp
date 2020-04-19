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

(defclass ssl-acceptor (acceptor)
  ((ssl-certificate-file :initarg :ssl-certificate-file
                         :reader acceptor-ssl-certificate-file
                         :documentation "A pathname designator for a
certificate file in PEM format.")
   (ssl-privatekey-file :initarg :ssl-privatekey-file
                        :reader acceptor-ssl-privatekey-file
                        :documentation "A pathname designator for a
private key file in PEM format, or \(only on LispWorks) NIL if the
certificate file contains the private key.")
   (ssl-privatekey-password :initform nil
                            :initarg :ssl-privatekey-password
                            :reader acceptor-ssl-privatekey-password
                            :documentation "The password for the
private key file or NIL for no password."))
  (:default-initargs
   :port 443)
  (:documentation "Create and START an instance of this class
\(instead of ACCEPTOR) if you want an https server.  There are two
required initargs, :SSL-CERTIFICATE-FILE and :SSL-PRIVATEKEY-FILE, for
pathname designators denoting the certificate file and the key file in
PEM format.  On LispWorks, you can have both in one file in which case
the second initarg is optional.  You can also use the
:SSL-PRIVATEKEY-PASSWORD initarg to provide a password \(as a string)
for the key file \(or NIL, the default, for no password).

The default port for SSL-ACCEPTOR instances is 443 instead of 80"))

;; general implementation

(defmethod acceptor-ssl-p ((acceptor ssl-acceptor))
  t)

(defmethod initialize-instance :after ((acceptor ssl-acceptor) &rest initargs)
  (declare (ignore initargs))
  ;; LispWorks can read both from the same file, so we can default one
  #+:lispworks
  (unless (slot-boundp acceptor 'ssl-privatekey-file)
    (setf (slot-value acceptor 'ssl-privatekey-file)
          (acceptor-ssl-certificate-file acceptor)))
  ;; OpenSSL doesn't know much about Lisp pathnames...
  (setf (slot-value acceptor 'ssl-privatekey-file)
        (namestring (truename (acceptor-ssl-privatekey-file acceptor)))
        (slot-value acceptor 'ssl-certificate-file)
        (namestring (truename (acceptor-ssl-certificate-file acceptor)))))

;; usocket implementation

#-:lispworks
(defmethod initialize-connection-stream ((acceptor ssl-acceptor) stream)
  ;; attach SSL to the stream if necessary
  (call-next-method acceptor
                    (cl+ssl:make-ssl-server-stream stream
                                                   :certificate (acceptor-ssl-certificate-file acceptor)
                                                   :key (acceptor-ssl-privatekey-file acceptor)
                                                   :password (acceptor-ssl-privatekey-password acceptor))))

;; LispWorks implementation

#+:lispworks
(defun make-ssl-server-stream (socket-stream &key certificate-file privatekey-file privatekey-password)
  "Given the acceptor socket stream SOCKET-STREAM attaches SSL to the
stream using the certificate file CERTIFICATE-FILE and the private key
file PRIVATEKEY-FILE.  Both of these values must be namestrings
denoting the location of the files and will be fed directly to
OpenSSL.  If PRIVATEKEY-PASSWORD is not NIL then it should be the
password for the private key file \(if necessary).  Returns the
stream."
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

#+:lispworks
(defmethod initialize-connection-stream ((acceptor ssl-acceptor) stream)
  ;; attach SSL to the stream if necessary
  (call-next-method acceptor
                    (make-ssl-server-stream stream
                                            :certificate-file (acceptor-ssl-certificate-file acceptor)
                                            :privatekey-file (acceptor-ssl-privatekey-file acceptor)
                                            :privatekey-password (acceptor-ssl-privatekey-password acceptor))))


#-:lispworks
(defun get-peer-ssl-certificate ()
  (cl+ssl:ssl-stream-x509-certificate *hunchentoot-stream*))
