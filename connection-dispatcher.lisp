;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; $Header$

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

;;; The connection-dispatcher protocol defines how Hunchentoot schedules
;;; request execution to worker threads or for inline execution.

(defclass connection-dispatcher ()
  ((server :initarg :server
           :reader server
           :documentation "The Hunchentoot server instance that this
connection dispatcher works for."))
  (:documentation "Base class for all connection dispatchers classes.
Its purpose is to carry the back pointer to the server instance."))

(defgeneric execute-acceptor (connection-dispatcher)
  (:documentation
   "This function is called once Hunchentoot has performed all initial
processing to start listening for incoming connections.  It does so by
calling the ACCEPT-CONNECTIONS functions of the server, taken from
the SERVER slot of the connection dispatcher instance.

In a multi-threaded environment, the connection dispatcher starts a new
thread and calls THUNK in that thread.  In a single-threaded
environment, the thunk will be called directly."))

(defgeneric handle-incoming-connection (connection-dispatcher socket)
  (:documentation
   "This function is called by Hunchentoot to start processing of
requests on a new incoming connection.  SOCKET is the usocket instance
that represents the new connection \(or a socket handle on LispWorks).
The connection dispatcher starts processing requests on the incoming
connection by calling the START-REQUEST-PROCESSING function of the
server instance, taken from the SERVER slot in the connection dispatcher
instance.  The SOCKET argument is passed to START-REQUEST-PROCESSING
as argument.

In a multi-threaded environment, the connection dispatcher runs this function
in a separate thread.  In a single-threaded environment, this function
is called directly."))

(defgeneric shutdown (connection-dispatcher)
  (:documentation "Terminate all threads that are currently associated
with the connection dispatcher, if any.")
  (:method ((manager t))
    #+:lispworks
    (when-let (acceptor (server-acceptor (server manager)))
      ;; kill the main acceptor process, see LW documentation for
      ;; COMM:START-UP-SERVER
      (mp:process-kill acceptor))))

(defclass single-threaded-connection-dispatcher (connection-dispatcher)
  ()
  (:documentation "Connection Dispatcher that runs synchronously in the
thread that invoked the START-SERVER function."))

(defmethod execute-acceptor ((manager single-threaded-connection-dispatcher))
  (accept-connections (server manager)))

(defmethod handle-incoming-connection ((manager single-threaded-connection-dispatcher) socket)
  (process-connection (server manager) socket))

(defclass one-thread-per-connection-dispatcher (connection-dispatcher)
  ((acceptor-process :accessor acceptor-process
                     :documentation "Process that accepts incoming
                     connections and dispatches them to new processes
                     for request execution."))
  (:documentation "Connection Dispatcher that starts one thread for
listening to incoming requests and one thread for each incoming
connection."))

(defmethod execute-acceptor ((manager one-thread-per-connection-dispatcher))
  #+:lispworks
  (accept-connections (server manager))
  #-:lispworks
  (setf (acceptor-process manager)
        (bt:make-thread (lambda ()
                          (accept-connections (server manager)))
                        :name (format nil "Hunchentoot acceptor \(~A:~A)"
                                      (or (server-address (server manager)) "*")
                                      (server-port (server manager))))))

#-:lispworks
(defmethod shutdown ((manager one-thread-per-connection-dispatcher))
  (loop
     while (bt:thread-alive-p (acceptor-process manager))
     do (sleep 1)))

#+:lispworks
(defmethod handle-incoming-connection ((manager one-thread-per-connection-dispatcher) handle)
  (incf *worker-counter*)
  ;; check if we need to perform a global GC
  (when (and *cleanup-interval*
             (zerop (mod *worker-counter* *cleanup-interval*)))
    (when *cleanup-function*
      (funcall *cleanup-function*)))
  (mp:process-run-function (format nil "Hunchentoot worker \(client: ~{~A:~A~})"
                                   (multiple-value-list
                                    (get-peer-address-and-port handle)))
                           nil #'process-connection
                           (server manager) handle))

#-:lispworks
(defun client-as-string (socket)
  (let ((address (usocket:get-peer-address socket))
        (port (usocket:get-peer-port socket)))
    (when (and address port)
      (format nil "~A:~A"
              (usocket:vector-quad-to-dotted-quad address)
              port))))

#-:lispworks
(defmethod handle-incoming-connection ((manager one-thread-per-connection-dispatcher) socket)
  (bt:make-thread (lambda ()
                    (process-connection (server manager) socket))
                  :name (format nil "Hunchentoot worker \(client: ~A)" (client-as-string socket))))
