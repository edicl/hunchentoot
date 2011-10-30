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

(defclass reply ()
  ((content-type :reader content-type
                 :documentation "The outgoing 'Content-Type' http
header which defaults to the value of *DEFAULT-CONTENT-TYPE*.")
   (content-length :reader content-length
                   :initform nil
                   :documentation "The outgoing 'Content-Length'
http header which defaults NIL.  If this is NIL, Hunchentoot will
compute the content length.")
   (headers-out :initform nil
                :reader headers-out
                :documentation "An alist of the outgoing http headers
not including the 'Set-Cookie', 'Content-Length', and 'Content-Type'
headers.  Use the functions HEADER-OUT and \(SETF HEADER-OUT) to
modify this slot.")
   (return-code :initform +http-ok+
                :accessor return-code
                :documentation "The http return code of this
reply.  The return codes Hunchentoot can handle are defined in
specials.lisp.")
   (external-format :initform *hunchentoot-default-external-format*
                    :accessor reply-external-format
                    :documentation "The external format of the reply -
used for character output.")
   (cookies-out :initform nil
                :accessor cookies-out
                :documentation "The outgoing cookies.  This slot's
value should only be modified by the functions defined in
cookies.lisp."))
  (:documentation "Objects of this class hold all the information
about an outgoing reply.  They are created automatically by
Hunchentoot and can be accessed and modified by the corresponding
handler.

You should not mess with the slots of these objects directly, but you
can subclass REPLY in order to implement your own behaviour.  See the
REPLY-CLASS slot of the ACCEPTOR class."))

(defmethod initialize-instance :after ((reply reply) &key)
  (setf (header-out :content-type reply) *default-content-type*))

(defun headers-out* (&optional (reply *reply*))
  "Returns an alist of the outgoing headers associated with the
REPLY object REPLY."
  (headers-out reply))

(defun cookies-out* (&optional (reply *reply*))
  "Returns an alist of the outgoing cookies associated with the
REPLY object REPLY."
  (cookies-out reply))

(defun (setf cookies-out*) (new-value &optional (reply *reply*))
  "Sets the alist of the outgoing cookies associated with the REPLY
object REPLY."
  (setf (cookies-out reply) new-value))

(defun content-type* (&optional (reply *reply*))
  "The outgoing 'Content-Type' http header of REPLY."
  (content-type reply))

(defun (setf content-type*) (new-value &optional (reply *reply*))
  "Sets the outgoing 'Content-Type' http header of REPLY."
  (setf (header-out :content-type reply) new-value))

(defun content-length* (&optional (reply *reply*))
  "The outgoing 'Content-Length' http header of REPLY."
  (content-length reply))

(defun (setf content-length*) (new-value &optional (reply *reply*))
  "Sets the outgoing 'Content-Length' http header of REPLY."
  (setf (header-out :content-length reply) new-value))

(defun return-code* (&optional (reply *reply*))
  "The http return code of REPLY.  The return codes Hunchentoot can
handle are defined in specials.lisp."
  (return-code reply))

(defun (setf return-code*) (new-value &optional (reply *reply*))
  "Sets the http return code of REPLY."
  (setf (return-code reply) new-value))

(defun reply-external-format* (&optional (reply *reply*))
  "The external format of REPLY which is used for character output."
  (reply-external-format reply))

(defun (setf reply-external-format*) (new-value &optional (reply *reply*))
  "Sets the external format of REPLY."
  (setf (reply-external-format reply) new-value))

(defun header-out-set-p (name &optional (reply *reply*))
  "Returns a true value if the outgoing http header named NAME has
been specified already.  NAME should be a keyword or a string."
  (assoc* name (headers-out reply)))

(defun header-out (name &optional (reply *reply*))
  "Returns the current value of the outgoing http header named NAME.
NAME should be a keyword or a string."
  (cdr (assoc name (headers-out reply))))

(defun cookie-out (name &optional (reply *reply*))
  "Returns the current value of the outgoing cookie named
NAME. Search is case-sensitive."
  (cdr (assoc name (cookies-out reply) :test #'string=)))

(defgeneric (setf header-out) (new-value name &optional reply)
  (:documentation "Changes the current value of the outgoing http
header named NAME \(a keyword or a string).  If a header with this
name doesn't exist, it is created.")
  (:method (new-value (name symbol) &optional (reply *reply*))
   ;; the default method
   (let ((entry (assoc name (headers-out reply))))
     (if entry
       (setf (cdr entry) new-value)
       (setf (slot-value reply 'headers-out)
             (acons name new-value (headers-out reply))))
     new-value))
  (:method (new-value (name string) &optional (reply *reply*))
   "If NAME is a string, it is converted to a keyword first."
   (setf (header-out (as-keyword name :destructivep nil) reply) new-value))
  (:method :after (new-value (name (eql :content-length)) &optional (reply *reply*))
   "Special case for the `Content-Length' header."
   (check-type new-value integer)
   (setf (slot-value reply 'content-length) new-value))
  (:method :after (new-value (name (eql :content-type)) &optional (reply *reply*))
   "Special case for the `Content-Type' header."
   (check-type new-value (or null string))
   (setf (slot-value reply 'content-type) new-value)))
