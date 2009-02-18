;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/server.lisp,v 1.43 2008/04/09 08:17:48 edi Exp $

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

(defclass acceptor ()
  ((port :initarg :port
         :reader acceptor-port
         :documentation "The port the acceptor is listening on.  The
default is 80.  Note that depending on your operating system you might
need special privileges to listen on port 80.")
   (address :initarg :address
            :reader acceptor-address
            :documentation "The address the acceptor is listening on.
If address is a string denoting an IP address, then the server only
receives connections for that address.  This must be one of the
addresses associated with the machine and allowed values are host
names such as \"www.zappa.com\" and address strings such as
\"72.3.247.29\".  If address is NIL, then the server will receive
connections to all IP addresses on the machine. This is the default.")
   (name :initarg :name
         :accessor acceptor-name
         :documentation "The optional name of the acceptor, a symbol.
This name can be utilized when defining \"easy handlers\" - see
DEFINE-EASY-HANDLER.  The default name is an uninterned symbol as
returned by GENSYM.")
   (request-class :initarg :request-class
                  :accessor acceptor-request-class
                  :documentation "Determines which class of request
objects is created when a request comes in and should be \(a symbol
naming) a class which inherits from REQUEST.  The default is the
symbol REQUEST.")
   (reply-class :initarg :reply-class
                :accessor acceptor-reply-class
                  :documentation "Determines which class of reply
objects is created when a request is served in and should be \(a
symbol naming) a class which inherits from REPLY.  The default is the
symbol REPLY.")
   (request-dispatcher :initarg :request-dispatcher
                       :accessor acceptor-request-dispatcher
                       :documentation "A designator for the request
dispatcher function used by this acceptor.  A function which accepts a
REQUEST object and calls a request handler of its choice \(and returns
its return value).  The default is the unexported symbol
LIST-REQUEST-DISPATCHER which works through the list
*DISPATCH-TABLE*.")
   (taskmaster :initarg :taskmaster
               :reader acceptor-taskmaster
               :documentation "The taskmaster \(i.e. an instance of a
subclass of TASKMASTER) that is responsible for scheduling the work
for this acceptor.  The default depends on the MP capabilities of the
underlying Lisp.")
   (output-chunking-p :initarg :output-chunking-p
                      :accessor acceptor-output-chunking-p
                      :documentation "A generalized boolean denoting
whether the acceptor may use chunked encoding for output, i.e. when
sending data to the client.  The default is T and there's usually no
reason to change this to NIL.")
   (input-chunking-p :initarg :input-chunking-p
                     :accessor acceptor-input-chunking-p
                      :documentation "A generalized boolean denoting
whether the acceptor may use chunked encoding for input, i.e. when
accepting request bodies from the client.  The default is T and
there's usually no reason to change this to NIL.")
   (persistent-connections-p :initarg :persistent-connections-p
                             :accessor acceptor-persistent-connections-p
                             :documentation "A generalized boolean
denoting whether the acceptor supports persistent connections, which
is the default for threaded acceptors.  If this property is NIL,
Hunchentoot closes each incoming connection after having processed one
request.  This is the default for non-threaded acceptors.")
   (read-timeout :initarg :read-timeout
                 :reader acceptor-read-timeout
                 :documentation "The read timeout of the acceptor,
specified in \(fractional) seconds.  The precise semantics of this
parameter is determined by the underlying Lisp's implementation of
socket timeouts.  NIL means no timeout.")
   (write-timeout :initarg :write-timeout
                  :reader acceptor-write-timeout
                  :documentation "The write timeout of the acceptor,
specified in \(fractional) seconds.  The precise semantics of this
parameter is determined by the underlying Lisp's implementation of
socket timeouts.  NIL means no timeout.")
   #+:lispworks
   (process :accessor acceptor-process
            :documentation "The Lisp process which accepts incoming
requests.  This is the process started by COMM:START-UP-SERVER and no
matter what kind of taskmaster you are using this will always be a new
process different from the one where START was called.")
   #-:lispworks
   (listen-socket :accessor acceptor-listen-socket
                  :documentation "The socket listening for incoming
connections.")
   (acceptor-shutdown-p :initform nil
                        :accessor acceptor-shutdown-p
                        :documentation "A flag that makes the acceptor
shutdown itself when set to something other than NIL.")
   (access-logger :initarg :access-logger
                  :accessor acceptor-access-logger
                  :documentation "Designator for a function to call to
log access to the acceptor.  The function must accept the RETURN-CODE,
CONTENT and CONTENT-LENGTH keyword arguments which are used to pass in
additional information about the request to log.  In addition, it can
use the standard request accessor functions that are available to
handler functions to find out more information about the request.
This slot defaults to a function which logs the information to the
file determined by *ACCESS-LOG-PATHNAME* \(unless that value is NIL)
in a format that can be parsed by most Apache log analysis tools.

If the value of this slot is NIL, access logging is turned off for
this acceptor.")
   (message-logger :initarg :message-logger
                   :accessor acceptor-message-logger
                   :documentation "Designator for a function to call
to log messages by the acceptor.  It must accept a severity level for
the message, which will be one of :ERROR, :INFO, or :WARNING, a format
string and an arbitary number of formatting arguments.  This slot
defaults to a function which writes to the file determined by
*MESSAGE-LOG-PATHNAME* \(unless that value is NIL).

If the value of this slot is NIL, message logging is turned off for
this acceptor."))
  (:default-initargs
   :address nil
   :port 80
   :name (gensym)
   :request-class 'request
   :reply-class 'reply
   :request-dispatcher 'list-request-dispatcher
   :taskmaster (make-instance (cond (*supports-threads-p* 'one-thread-per-connection-taskmaster)
                                    (t 'single-threaded-taskmaster)))
   :output-chunking-p t
   :input-chunking-p t
   :persistent-connections-p t
   :read-timeout *default-connection-timeout*
   :write-timeout *default-connection-timeout*
   :access-logger 'log-access-to-file
   :message-logger 'log-message-to-file)
  (:documentation "To create a Hunchentoot webserver, you make an
instance of this class and use the generic function START to start it
\(and STOP to stop it).  Use the :PORT initarg if you don't want to
listen on the default http port 80.  There are other initargs most of
which you probably won't need very often.  They are explained in
detail in the docstrings of the slot definitions for this class.

Unless you are in a Lisp without MP capabilities, you can have several
active instances of ACCEPTOR \(listening on different ports) at the
same time."))

(defmethod print-object ((acceptor acceptor) stream)
  (print-unreadable-object (acceptor stream :type t)
    (format stream "\(host ~A, port ~A)"
            (or (acceptor-address acceptor) "*") (acceptor-port acceptor))))

(defgeneric start (acceptor)
  (:documentation "Starts the ACCEPTOR so that it begins accepting
connections.  Returns the acceptor."))

(defgeneric stop (acceptor)
  (:documentation "Stops the ACCEPTOR so that it no longer accepts
requests."))

(defgeneric start-listening (acceptor)
  (:documentation "Sets up a listen socket for the given ACCEPTOR and
enables it to listen to incoming connections.  This function is called
from the thread that starts the acceptor initially and may return
errors resulting from the listening operation \(like 'address in use'
or similar)."))

(defgeneric accept-connections (acceptor)
  (:documentation "In a loop, accepts a connection and hands it over
to the acceptor's taskmaster for processing using
HANDLE-INCOMING-CONNECTION.  On LispWorks, this function returns
immediately, on other Lisps it retusn only once the acceptor has been
stopped."))

(defgeneric initialize-connection-stream (acceptor stream)
 (:documentation "Can be used to modify the stream which is used to
communicate between client and server before the request is read.  The
default method of ACCEPTOR does nothing, but see for example the
method defined for SSL-ACCEPTOR.  All methods of this generic function
must return the stream to use."))

(defgeneric reset-connection-stream (acceptor stream)
  (:documentation "Resets the stream which is used to communicate
between client and server after one request has been served so that it
can be used to process the next request.  This generic function is
called after a request has been processed and must return the
stream."))

(defgeneric process-connection (acceptor socket)
  (:documentation "This function is called by the taskmaster when a
new client connection has been established.  Its arguments are the
ACCEPTOR object and a LispWorks socket handle or a usocket socket
stream object in SOCKET.  It reads the request headers, sets up the
request and reply objects, and hands over to PROCESS-REQUEST.  This is
done in a loop until the stream has to be closed or until a connection
timeout occurs.

It is probably not a good idea to re-implement this method until you
really, really know what you're doing, but you can for example write
an around method specialized for your subclass of ACCEPTOR which binds
or rebinds special variables which can then be accessed by your
handlers."))

(defgeneric acceptor-ssl-p (acceptor) 
  (:documentation "Returns a true value if ACCEPTOR uses SSL
connections.  The default is to unconditionally return NIL and
subclasses of ACCEPTOR must specialize this method to signal that
they're using secure connections - see the SSL-ACCEPTOR class."))

;; general implementation

(defmethod start ((acceptor acceptor))
  (start-listening acceptor)
  (let ((taskmaster (acceptor-taskmaster acceptor)))
    (setf (taskmaster-acceptor taskmaster) acceptor)
    (execute-acceptor taskmaster))
  acceptor)

(defmethod stop ((acceptor acceptor))
  (setf (acceptor-shutdown-p acceptor) t)
  (shutdown (acceptor-taskmaster acceptor))
  #-:lispworks
  (usocket:socket-close (acceptor-listen-socket acceptor))
  acceptor)

(defmethod initialize-connection-stream ((acceptor acceptor) stream)
 (declare (ignore acceptor))
 ;; default method does nothing
 stream)

(defmethod reset-connection-stream ((acceptor acceptor) stream)
  (declare (ignore acceptor))
  ;; turn chunking off at this point
  (cond ((typep stream 'chunked-stream)
         ;; flush the stream first and check if there's unread input
         ;; which would be an error
         (setf (chunked-stream-output-chunking-p stream) nil
               (chunked-stream-input-chunking-p stream) nil)
         ;; switch back to bare socket stream
         (chunked-stream-stream stream))
        (t stream)))

(defmethod process-connection :around ((*acceptor* acceptor) (socket t))
  ;; this around method is used for error handling
  (declare (ignore socket))
  ;; note that this method also binds *ACCEPTOR*
  (handler-bind ((error
                  ;; abort if there's an error which isn't caught inside
                  (lambda (cond)
                    (log-message *lisp-errors-log-level*
                                 "Error while processing connection: ~A" cond)
                    (return-from process-connection)))
                 (warning
                  ;; log all warnings which aren't caught inside
                  (lambda (cond)
                    (log-message *lisp-warnings-log-level*
                                 "Warning while processing connection: ~A" cond))))
    (call-next-method)))

(defmethod process-connection ((*acceptor* acceptor) (socket t))
  (let ((*hunchentoot-stream*
         (initialize-connection-stream *acceptor* (make-socket-stream socket *acceptor*))))
    (unwind-protect
        ;; process requests until either the acceptor is shut down,
        ;; *CLOSE-HUNCHENTOOT-STREAM* has been set to T by the
        ;; handler, or the peer fails to send a request
        (loop
         (let ((*close-hunchentoot-stream* t))
           (when (acceptor-shutdown-p *acceptor*)
             (return))
           (multiple-value-bind (headers-in method url-string protocol)
               (get-request-data *hunchentoot-stream*)
             ;; check if there was a request at all
             (unless method
               (return))
             ;; bind per-request special variables, then process the
             ;; request - note that *ACCEPTOR* was bound above already
             (let ((*reply* (make-instance (acceptor-reply-class *acceptor*)))
                   (*session* nil)
                   (transfer-encodings (cdr (assoc* :transfer-encoding headers-in))))
               (when transfer-encodings
                 (setq transfer-encodings
                       (split "\\s*,\\*" transfer-encodings))
                 (when (member "chunked" transfer-encodings :test #'equalp)
                   (cond ((acceptor-input-chunking-p *acceptor*)
                          ;; turn chunking on before we read the request body
                          (setf *hunchentoot-stream* (make-chunked-stream *hunchentoot-stream*)
                                (chunked-stream-input-chunking-p *hunchentoot-stream*) t))
                         (t (hunchentoot-error "Client tried to use ~
chunked encoding, but acceptor is configured to not use it.")))))
               (multiple-value-bind (remote-addr remote-port)
                   (get-peer-address-and-port socket)
                 (process-request (make-instance (acceptor-request-class *acceptor*)
                                                 :acceptor *acceptor*
                                                 :remote-addr remote-addr
                                                 :remote-port remote-port
                                                 :headers-in headers-in
                                                 :content-stream *hunchentoot-stream*
                                                 :method method
                                                 :uri url-string
                                                 :server-protocol protocol))))
             (force-output *hunchentoot-stream*)
             (setq *hunchentoot-stream* (reset-connection-stream *acceptor* *hunchentoot-stream*))
             (when *close-hunchentoot-stream*
               (return)))))
      (when *hunchentoot-stream*
        ;; as we are at the end of the request here, we ignore all
        ;; errors that may occur while flushing and/or closing the
        ;; stream.
        (ignore-errors
          (force-output *hunchentoot-stream*)
          (close *hunchentoot-stream* :abort t))))))
  
(defmethod acceptor-ssl-p ((acceptor t))
  ;; the default is to always answer "no"
  nil)
	 	 
;; usocket implementation

#-:lispworks
(defmethod start-listening ((acceptor acceptor))
  (setf (acceptor-listen-socket acceptor)
        (usocket:socket-listen (or (acceptor-address acceptor)
                                   usocket:*wildcard-host*)
                               (acceptor-port acceptor)
                               :reuseaddress t
                               :element-type '(unsigned-byte 8)))
  (values))

#-:lispworks
(defmethod accept-connections ((acceptor acceptor))
  (usocket:with-server-socket (listener (acceptor-listen-socket acceptor))
    (loop
     (when (acceptor-shutdown-p acceptor)
       (return))
     (when (usocket:wait-for-input listener :timeout +new-connection-wait-time+)
       (handler-case
           (when-let (client-connection (usocket:socket-accept listener))
             (set-timeouts client-connection
                           (acceptor-read-timeout acceptor)
                           (acceptor-write-timeout acceptor))
             (handle-incoming-connection (acceptor-taskmaster acceptor)
                                         client-connection))
         ;; ignore condition
         (usocket:connection-aborted-error ()))))))

;; LispWorks implementation

#+:lispworks
(defmethod start-listening ((acceptor acceptor))
  (multiple-value-bind (listener-process startup-condition)
      (comm:start-up-server :service (acceptor-port acceptor)
                            :address (acceptor-address acceptor)
                            :process-name (format nil "Hunchentoot listener \(~A:~A)"
                                                  (or (acceptor-address acceptor) "*")
                                                  (acceptor-port acceptor))
                            ;; this function is called once on startup - we
                            ;; use it to check for errors
                            :announce (lambda (socket &optional condition)
                                        (declare (ignore socket))
                                        (when condition
                                          (error condition)))
                            ;; this function is called whenever a connection
                            ;; is made
                            :function (lambda (handle)
                                        (unless (acceptor-shutdown-p acceptor)
                                          (handle-incoming-connection
                                           (acceptor-taskmaster acceptor) handle)))
                            ;; wait until the acceptor was successfully started
                            ;; or an error condition is returned
                            :wait t)
    (when startup-condition
      (error startup-condition))
    (mp:process-stop listener-process)
    (setf (acceptor-process acceptor) listener-process)
    (values)))

#+:lispworks
(defmethod accept-connections ((acceptor acceptor))
  (mp:process-unstop (acceptor-process acceptor))
  nil)

(defun list-request-dispatcher (request)
  "The default request dispatcher which selects a request handler
based on a list of individual request dispatchers all of which can
either return a handler or neglect by returning NIL."
  (loop for dispatcher in *dispatch-table*
        for action = (funcall dispatcher request)
        when action return (funcall action)
        finally (setf (return-code *reply*) +http-not-found+)))

