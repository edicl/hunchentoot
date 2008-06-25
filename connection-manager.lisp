;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
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

;;; The connection-manager protocol defines how Hunchentoot schedules
;;; request execution to worker threads or for inline execution.

(defclass connection-manager ()
  ((server :initarg :server
           :reader server
           :documentation "The Hunchentoot server instance that this
connection manager works for."))
  (:documentation "Base class for all connection managers classes.
Its purpose is to carry the back pointer to the server instance."))

(defgeneric execute-listener (connection-manager)
  (:documentation
   "This function is called once Hunchentoot has performed all initial
processing to start listening for incoming connections.  It does so by
calling the LISTEN-FOR-CONNECTIONS functions of the server, taken from
the SERVER slot of the connection manager instance.

In a multi-threaded environment, the connection manager starts a new
thread and calls THUNK in that thread.  In a single-threaded
environment, the thunk will be called directly."))

(defgeneric handle-incoming-connection (connection-manager socket)
  (:documentation
   "This function is called by Hunchentoot to start processing of
requests on a new incoming connection.  SOCKET is the usocket instance
that represents the new connection \(or a socket handle on LispWorks).
The connection manager starts processing requests on the incoming
connection by calling the START-REQUEST-PROCESSING function of the
server instance, taken from the SERVER slot in the connection manager
instance.  The SOCKET argument is passed to START-REQUEST-PROCESSING
as argument.

In a multi-threaded environment, the connection manager runs the thunk
in a separate thread.  In a single-threaded environment, the thunk
will be called directly."))

(defgeneric shutdown (connection-manager)
  (:documentation "Terminate all threads that are currently associated
with the connection manager, if any.")
  (:method (manager)
    (declare (ignore manager))
    #+:lispworks
    (when-let (listener (server-listener (server manager)))
      ;; kill the main listener process, see LW documentation for
      ;; COMM:START-UP-SERVER
      (mp:process-kill listener))))

(defclass single-threaded-connection-manager (connection-manager)
  ()
  (:documentation "Connection manager that runs synchronously in the
thread that invoked the START-SERVER function."))

(defmethod execute-listener ((manager single-threaded-connection-manager))
  (listen-for-connections (server manager)))

(defmethod handle-incoming-connection ((manager single-threaded-connection-manager) socket)
  (process-connection (server manager) socket))

(defclass one-thread-per-connection-manager (connection-manager)
  ((workers :initform nil
            :accessor connection-manager-workers
            :documentation "A list of currently active worker
threads."))
  (:documentation "Connection manager that starts one thread for
listening to incoming requests and one thread for each incoming
connection."))

(defmethod print-object ((manager one-thread-per-connection-manager) stream)
  (print-unreadable-object (manager stream :type t)
    (format stream "~A worker~:P" (length (connection-manager-workers manager)))))

(defmethod execute-listener ((manager one-thread-per-connection-manager))
  #+:lispworks
  (listen-for-connections (server manager))
  #-:lispworks
  (bt:make-thread (lambda ()
                    (listen-for-connections (server manager)))
                  :name (format nil "Hunchentoot listener \(~A:~A)"
                                (or (server-address (server manager)) "*")
                                (server-port (server manager)))))

#+:lispworks
(defmethod handle-incoming-connection ((manager one-thread-per-connection-manager) handle)
  (incf *worker-counter*)
  ;; check if we need to perform a global GC
  (when (and *cleanup-interval*
             (zerop (mod *worker-counter* *cleanup-interval*)))
    (when *cleanup-function*
      (funcall *cleanup-function*)))
  ;; start a worker thread for this connection and remember it
  (push (mp:process-run-function (format nil "Hunchentoot worker \(client: ~{~A:~A~})"
                                         (multiple-value-list
                                          (get-peer-address-and-port handle)))
                                 nil #'process-connection
                                 (server manager) handle)
        (connection-manager-workers manager)))

#-:lispworks
(defmethod handle-incoming-connection ((manager one-thread-per-connection-manager) socket)
  (push (bt:make-thread (lambda ()
                          (process-connection (server manager) socket))
                        :name (format nil "Hunchentoot worker \(client: ~A:~A)"
                                      (usocket:vector-quad-to-dotted-quad (usocket:get-peer-address socket))
                                      (usocket:get-peer-port socket)))
        (connection-manager-workers manager)))

#+:lispworks
(defmethod shutdown ((manager one-thread-per-connection-manager))
  ;; kill all worker threads
  (dolist (worker (connection-manager-workers manager))
    (ignore-errors
      (when (mp:process-alive-p worker)
        (mp:process-kill worker)))
    (mp:process-allow-scheduling))
  ;; finally, kill main listener
  (call-next-method))
