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

(defclass taskmaster ()
  ((acceptor :accessor taskmaster-acceptor
             :documentation "A backpointer to the acceptor instance
this taskmaster works for."))
  (:documentation "An instance of this class is responsible for
distributing the work of handling requests for its acceptor.  This is
an \"abstract\" class in the sense that usually only instances of
subclasses of TASKMASTER will be used."))

(defgeneric execute-acceptor (taskmaster)
  (:documentation "This is a callback called by the acceptor once it
has performed all initial processing to start listening for incoming
connections \(see START-LISTENING).  It usually calls the
ACCEPT-CONNECTIONS method of the acceptor, but depending on the
taskmaster instance the method might be called from a new thread."))

(defgeneric handle-incoming-connection (taskmaster socket)
  (:documentation "This function is called by the acceptor to start
processing of requests on a new incoming connection.  SOCKET is the
usocket instance that represents the new connection \(or a socket
handle on LispWorks).  The taskmaster starts processing requests on
the incoming connection by calling the PROCESS-CONNECTION method of
the acceptor instance.  The SOCKET argument is passed to
PROCESS-CONNECTION as an argument."))

(defgeneric shutdown (taskmaster)
  (:documentation "Shuts down the taskmaster, i.e. frees all resources
that were set up by it.  For example, a multi-threaded taskmaster
might terminate all threads that are currently associated with it.
This function is called by the acceptor's STOP method."))

(defclass single-threaded-taskmaster (taskmaster)
  ()
  (:documentation "A taskmaster that runs synchronously in the thread
where the START function was invoked \(or in the case of LispWorks in
the thread started by COMM:START-UP-SERVER).  This is the simplest
possible taskmaster implementation in that its methods do nothing but
calling their acceptor \"sister\" methods - EXECUTE-ACCEPTOR calls
ACCEPT-CONNECTIONS, HANDLE-INCOMING-CONNECTION calls
PROCESS-CONNECTION."))

(defmethod execute-acceptor ((taskmaster single-threaded-taskmaster))
  ;; in a single-threaded environment we just call ACCEPT-CONNECTIONS
  (accept-connections (taskmaster-acceptor taskmaster)))

(defmethod handle-incoming-connection ((taskmaster single-threaded-taskmaster) socket)
  ;; in a single-threaded environment we just call PROCESS-CONNECTION
  (process-connection (taskmaster-acceptor taskmaster) socket))

(defclass one-thread-per-connection-taskmaster (taskmaster)
  (#-:lispworks
   (acceptor-process :accessor acceptor-process
                     :documentation "A process that accepts incoming
connections and hands them off to new processes for request
handling."))
  (:documentation "A taskmaster that starts one thread for listening
to incoming requests and one thread for each incoming connection.

This is the default taskmaster implementation for multi-threaded Lisp
implementations."))

;; usocket implementation

#-:lispworks
(defmethod shutdown ((taskmaster taskmaster))
  taskmaster)

#-:lispworks
(defmethod shutdown ((taskmaster one-thread-per-connection-taskmaster))
  ;; just wait until the acceptor process has finished, then return
  (loop
   (unless (bt:thread-alive-p (acceptor-process taskmaster))
     (return))
   (sleep 1))
  taskmaster)

#-:lispworks
(defmethod execute-acceptor ((taskmaster one-thread-per-connection-taskmaster))
  (setf (acceptor-process taskmaster)
        (bt:make-thread (lambda ()
                          (accept-connections (taskmaster-acceptor taskmaster)))
                        :name (format nil "Hunchentoot listener \(~A:~A)"
                                      (or (acceptor-address (taskmaster-acceptor taskmaster)) "*")
                                      (acceptor-port (taskmaster-acceptor taskmaster))))))

#-:lispworks
(defun client-as-string (socket)
  "A helper function which returns the client's address and port as a
string and tries to act robustly in the presence of network problems."
  (let ((address (usocket:get-peer-address socket))
        (port (usocket:get-peer-port socket)))
    (when (and address port)
      (format nil "~A:~A"
              (usocket:vector-quad-to-dotted-quad address)
              port))))

#-:lispworks
(defmethod handle-incoming-connection ((taskmaster one-thread-per-connection-taskmaster) socket)
  (bt:make-thread (lambda ()
                    (process-connection (taskmaster-acceptor taskmaster) socket))
                  :name (format nil "Hunchentoot worker \(client: ~A)" (client-as-string socket))))

;; LispWorks implementation

#+:lispworks
(defmethod shutdown ((taskmaster taskmaster))
  (when-let (process (acceptor-process (taskmaster-acceptor taskmaster)))
    ;; kill the main acceptor process, see LW documentation for
    ;; COMM:START-UP-SERVER
    (mp:process-kill process))
  taskmaster)

#+:lispworks
(defmethod execute-acceptor ((taskmaster one-thread-per-connection-taskmaster))
  (accept-connections (taskmaster-acceptor taskmaster)))

#+:lispworks
(defmethod handle-incoming-connection ((taskmaster one-thread-per-connection-taskmaster) handle)
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
                           (taskmaster-acceptor taskmaster) handle))
