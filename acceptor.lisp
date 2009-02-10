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
         :documentation "The port the acceptor is listening on.
See START-SERVER.")
   (address :initarg :address
            :reader acceptor-address
            :documentation "The address the acceptor is listening
on.  See START-SERVER.")
   (name :initarg :name
         :accessor acceptor-name
         :documentation "The optional name of the acceptor, a symbol.")
   (request-class :initarg :request-class
                  :reader acceptor-request-class
                  :documentation "Determines which class of request
objects is created when a request comes in and should be \(a symbol
naming) a class which inherits from REQUEST.")
   (dispatch-table :initarg :dispatch-table
                   :accessor acceptor-dispatch-table
                   :documentation "The dispatch-table used by this
acceptor.  Can be NIL to denote that *DISPATCH-TABLE* should be used.")
   (output-chunking-p :initarg :output-chunking-p
                      :reader acceptor-output-chunking-p
                      :documentation "Whether the acceptor may use output chunking.")
   (input-chunking-p :initarg :input-chunking-p
                     :reader acceptor-input-chunking-p
                     :documentation "Whether the acceptor may use input chunking.")
   (persistent-connections-p :initarg :persistent-connections-p
                             :accessor acceptor-persistent-connections-p
                             :documentation "Whether the acceptor
supports persistent connections, which is the default for threaded
acceptors.  If this property is false, Hunchentoot closes incoming
connections after having processed one request.  This is the default
for non-threaded acceptors.")
   (read-timeout :initarg :read-timeout
                 :reader acceptor-read-timeout
                 :documentation "The connection timeout of the acceptor,
specified in (fractional) seconds.  Connections that are idle for
longer than this time are closed by Hunchentoot.  The precise
semantics of this parameter is determined by the underlying Lisp's
implementation of socket timeouts.")
   (write-timeout :initarg :write-timeout
                 :reader acceptor-write-timeout
                 :documentation "The connection timeout of the acceptor,
specified in (fractional) seconds.  The precise semantics of this
parameter is determined by the underlying Lisp's implementation of
socket timeouts.")
   (connection-dispatcher :initarg :connection-dispatcher
                       :initform nil
                       :reader acceptor-connection-dispatcher
                       :documentation "The connection dispatcher that is
responsible for listening to new connections and scheduling them for
execution.")
   #+:lispworks
   (acceptor :accessor acceptor-acceptor
             :documentation "The Lisp process which accepts incoming
requests.")
   #-:lispworks
   (listen-socket :accessor acceptor-listen-socket
                  :documentation "The listen socket for incoming
                  connections.")
   (acceptor-shutdown-p :initform nil
                      :accessor acceptor-shutdown-p
                      :documentation "Flag that makes the acceptor
shutdown itself when set to something other than NIL.")
   (access-logger :initarg :access-logger
                  :accessor acceptor-access-logger
                  :documentation "Designator for a function to call to
log access to the acceptor.  The function must accept the RETURN-CODE,
CONTENT and CONTENT-LENGTH keyword arguments which are used to pass in
additional information about the request to log.  In addition, it can
use the standard request accessor functions that are available to
handler functions to find out more information about the request.
This slot defaults to the LOG-ACCESS function which logs the
information to a file in a format that can be parsed by most Apache
log analysis tools.")
   (message-logger :initarg :message-logger
                   :accessor acceptor-message-logger
                   :documentation "Designator for a function to call
to log messages by the acceptor.  It must accept a severity level for
the message, which will be one of :NOTICE, :INFO, or :WARNING, a
format string and an arbitary number of formatting arguments.  This
slot defaults to the LOG-MESSAGE function which writes writes the
information to a file."))
  (:default-initargs
   :address nil
   :port 80
   :name (gensym)
   :request-class 'request
   :output-chunking-p t
   :input-chunking-p t
   :dispatch-table nil
   :access-logger 'log-access
   :message-logger 'log-message)
  (:documentation "An object of this class contains all relevant
information about a running Hunchentoot acceptor instance."))

(defmethod initialize-instance :after ((acceptor acceptor)
                                       &key connection-dispatcher-class
                                            connection-dispatcher-arguments
                                            (threaded *supports-threads-p* threaded-specified-p)
                                            (persistent-connections-p
                                             threaded
                                             persistent-connections-specified-p)
                                            (connection-timeout
                                             *default-connection-timeout*
                                             connection-timeout-provided-p)
                                            (read-timeout nil read-timeout-provided-p)
                                            (write-timeout nil write-timeout-provided-p))
  "The CONNECTION-DISPATCHER-CLASS and CONNECTION-DISPATCHER-ARGUMENTS
arguments to the creation of a acceptor instance determine the
connection dispatcher instance that is created.  THREADED is the user
friendly version of the CONNECTION-DISPATCHER-CLASS option.  If it is
NIL, an unthreaded connection dispatcher is used.  It is an error to
specify both THREADED and a CONNECTION-DISPATCHER-CLASS argument.

The PERSISTENT-CONNECTIONS-P keyword argument defaults to the value of
the THREADED keyword argument but can be overridden.

If a neither READ-TIMEOUT nor WRITE-TIMEOUT are specified by the user,
the acceptor's read and write timeouts default to the CONNECTION-TIMEOUT
value.  If either of READ-TIMEOUT or WRITE-TIMEOUT is specified,
CONNECTION-TIMEOUT is not used and may not be supplied."
  (declare (ignore read-timeout write-timeout))
  (when (and threaded-specified-p connection-dispatcher-class)
    (parameter-error "Can't use both THREADED and CONNECTION-DISPATCHER-CLASS arguments."))
  (unless persistent-connections-specified-p
    (setf (acceptor-persistent-connections-p acceptor) persistent-connections-p))
  (unless (acceptor-connection-dispatcher acceptor)
    (setf (slot-value acceptor 'connection-dispatcher)
          (apply #'make-instance
                 (or connection-dispatcher-class
                     (if threaded
                         'one-thread-per-connection-dispatcher
                         'single-threaded-connection-dispatcher))
                 :acceptor acceptor
                 connection-dispatcher-arguments)))
  (if (or read-timeout-provided-p write-timeout-provided-p)
      (when connection-timeout-provided-p
        (parameter-error "Can't have both CONNECTION-TIMEOUT and either of READ-TIMEOUT and WRITE-TIMEOUT."))
      (setf (slot-value acceptor 'read-timeout) connection-timeout
            (slot-value acceptor 'write-timeout) connection-timeout)))

(defgeneric acceptor-ssl-p (acceptor)
  (:documentation "Returns a true value if ACCEPTOR is an SSL acceptor.")
  (:method ((acceptor t))
    nil))

(defun ssl-p (&optional (acceptor *acceptor*))
  (acceptor-ssl-p acceptor))

(defmethod print-object ((acceptor acceptor) stream)
  (print-unreadable-object (acceptor stream :type t)
    (format stream "\(host ~A, port ~A)"
            (or (acceptor-address acceptor) "*") (acceptor-port acceptor))))

(defgeneric start (acceptor)
  (:documentation "Start the ACCEPTOR so that it begins accepting
connections.")
  (:method ((acceptor acceptor))
    (start-listening acceptor)
    (execute-acceptor (acceptor-connection-dispatcher acceptor))))

(defgeneric stop (acceptor)
  (:documentation "Stop the ACCEPTOR so that it does no longer accept requests.")
  (:method ((acceptor acceptor))
   (setf (acceptor-shutdown-p acceptor) t)
   (shutdown (acceptor-connection-dispatcher acceptor))
   #-:lispworks
   (usocket:socket-close (acceptor-listen-socket acceptor))))

(defun start-server (&rest args
                     &key port address dispatch-table name
                     threaded
                     input-chunking-p connection-timeout
                     persistent-connections-p
                     read-timeout write-timeout
                     #-:hunchentoot-no-ssl #-:hunchentoot-no-ssl #-:hunchentoot-no-ssl
                     ssl-certificate-file ssl-privatekey-file ssl-privatekey-password
                     access-logger)
  ;; except for SSL-CERTIFICATE-FILE, which is used to determine
  ;; whether SSL is desired, all arguments are here so that the lambda
  ;; list is self documenting and ignored otherwise
  (declare (ignore port address dispatch-table name
                   threaded
                   input-chunking-p connection-timeout
                   persistent-connections-p
                   read-timeout write-timeout
                   #-:hunchentoot-no-ssl #-:hunchentoot-no-ssl
                   ssl-privatekey-file ssl-privatekey-password
                   access-logger))
  "Starts a Hunchentoot server and returns the SERVER object \(which
can be stopped with STOP-SERVER).  PORT is the port the server will be
listening on - the default is 80 \(or 443 if SSL information is
provided).  If ADDRESS is a string denoting an IP address, then the
server only receives connections for that address.  This must be one
of the addresses associated with the machine and allowed values are
host names such as \"www.nowhere.com\" and address strings like
\"204.71.177.75\".  If ADDRESS is NIL, then the server will receive
connections to all IP addresses on the machine.  This is the default.

DISPATCH-TABLE can either be a dispatch table which is to be used by
this server or NIL which means that the value of the global variable
*DISPATCH-TABLE* at request time will be used.  This argument is of
course meaningless if you implement your own dispatch mechanism.

NAME should be a symbol which can be used to name the server.  This
name can utilized when defining \"easy handlers\" - see
DEFINE-EASY-HANDLER.  The default name is an uninterned symbol as
returned by GENSYM.

The REQUEST-CLASS argument determines which class of request objects
is created when a request comes in and should be \(a symbol naming) a
class which inherits from REQUEST.  The default is the symbol REQUEST.

If INPUT-CHUNKING-P is true, the server will accept request bodies
without a `Content-Length' header if the client uses chunked transfer
encoding.

If PERSISTENT-CONNECTIONS-P is true, the server will support
persistent connections and process multiple requests on one incoming
connection.  If it is false, Hunchentoot will close every connection
after one request has been processed.  This argument defaults to true
for threaded and false for non-threaded servers.

CONNECTION-TIMEOUT specifies the connection timeout for client
connections in \(fractional) seconds - use NIL for no timeout at all.
This parameter limits the time that Hunchentoot will wait for data to
be received from or sent to a client.  The details of how this
parameter works is implementation specific.

READ-TIMEOUT and WRITE-TIMEOUT specify implementation specific
timeouts for reading from and writing to client sockets.  The exact
semantics of these two parameters are Lisp implementation specific,
and not all implementations provide for separate read and write
timeout parameter setting.

CONNECTION-DISPATCHER-CLASS specifies the name of the class to instantiate
for managing how connections are mapped to threads.  You don't normally
want to specify this argument unless you want to have non-standard
threading behavior.   See the documentation for more information.

MESSAGE-LOGGER is a designator for a function to call to log messages
by the server.  It must accept a severity level for the message \(one
of :INFO, :WARNING, or :ERROR), a format string, and an arbitary
number of formatting arguments.  This slot defaults to the LOG-MESSAGE
function which writes writes the information to a file.

ACCESS-LOGGER is a designator for a function that is called by the
server to log requests.  It defaults to the function
HUNCHENTOOT::LOG-ACCESS and can be overriden for individual servers.
The function needs to accept the RETURN-CODE, CONTENT and
CONTENT-LENGTH keyword arguments which are bound by the server to the
HTTP return code, the CONTENT sent back to the client and the number
of bytes sent back in the request body to the client.
HUNCHENTOOT::LOG-ACCESS calls the generic logging function specified
by LOGGER.

If you want your server to use SSL you must provide the pathname
designator\(s) SSL-CERTIFICATE-FILE for the certificate file and
optionally SSL-PRIVATEKEY-FILE for the private key file, both files
must be in PEM format.  If you only provide the value for
SSL-CERTIFICATE-FILE it is assumed that both the certificate and the
private key are in one file.  If your private key needs a password you
can provide it through the SSL-PRIVATEKEY-PASSWORD keyword argument,
but this works only on LispWorks - for other Lisps the key must not be
associated with a password."
  (unless (boundp '*session-secret*)
    (reset-session-secret))
  #+:hunchentoot-no-ssl
  (when ssl-certificate-file
    (parameter-error "Hunchentoot SSL support is not compiled in."))
  (let ((server (apply #'make-instance
                       #-:hunchentoot-no-ssl
                       (if ssl-certificate-file 'ssl-acceptor 'acceptor)
                       #+:hunchentoot-no-ssl
                       'server
                       args)))
    (start server)
    server))

(defun stop-server (server)
  "Stops the Hunchentoot server SERVER."
  (stop server))

;; connection dispatcher API

(defgeneric start-listening (acceptor)
  (:documentation "Sets up a listen socket for the given ACCEPTOR and
enables it to listen for incoming connections.  This function is
called from the thread that starts the acceptor initially and may return
errors resulting from the listening operation. (like 'address in use'
or similar).")
  (:method ((acceptor acceptor))
    #+:lispworks
    (multiple-value-bind (listener-process startup-condition)
        (comm:start-up-server :service (acceptor-port acceptor)
                              :address (acceptor-address acceptor)
                              :process-name (format nil "Hunchentoot listener \(~A:~A)"
                                                    (or (acceptor-address acceptor) "*") (acceptor-port acceptor))
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
                                             (acceptor-connection-dispatcher acceptor) handle)))
                              ;; wait until the acceptor was successfully started
                              ;; or an error condition is returned
                              :wait t)
      (when startup-condition
        (error startup-condition))
      (mp:process-stop listener-process)
      (setf (acceptor-acceptor acceptor) listener-process))
    #-:lispworks
    (setf (acceptor-listen-socket acceptor)
          (usocket:socket-listen (or (acceptor-address acceptor)
                                     usocket:*wildcard-host*)
                                 (acceptor-port acceptor)
                                 :reuseaddress t
                                 :element-type '(unsigned-byte 8)))))

(defgeneric accept-connections (acceptor)
  (:documentation "In a loop, accepts a connection and
dispatches it to the acceptor's connection dispatcher object for processing
using HANDLE-INCOMING-CONNECTION.")
  (:method ((acceptor acceptor))
    #+:lispworks
    (mp:process-unstop (acceptor-acceptor acceptor))
    #-:lispworks
    (usocket:with-acceptor-socket (listener (acceptor-listen-socket acceptor))
      (loop
         until (acceptor-shutdown-p acceptor)
         when (usocket:wait-for-input listener :timeout +new-connection-wait-time+)
         do (handler-case
                (when-let (client-connection (usocket:socket-accept listener))
                  (set-timeouts client-connection
                                (acceptor-read-timeout acceptor)
                                (acceptor-write-timeout acceptor))
                  (handle-incoming-connection (acceptor-connection-dispatcher acceptor)
                                              client-connection))
              ;; ignore condition
              (usocket:connection-aborted-error ()))))))

(defgeneric initialize-connection-stream (acceptor stream) 
 (:documentation "Wraps the given STREAM with all the additional
stream classes to support the functionality required by ACCEPTOR.  The
methods of this generic function must return the stream to use.")
 ;; default method does nothing
 (:method (acceptor stream)
  (declare (ignore acceptor))
  stream))

(defgeneric reset-connection-stream (acceptor stream)
  (:documentation "Resets the given STREAM so that it can be used to
process the next request, ACCEPTOR is the acceptor that this stream
belongs to, which determines what to do to reset.  This generic
function is called after a request has been processed and must return
the stream.")
  (:method (acceptor stream)
    (declare (ignore acceptor))
    ;; turn chunking off at this point
    (cond ((typep stream 'chunked-stream)
           ;; flush the stream first and check if there's unread input
           ;; which would be an error
           (setf (chunked-stream-output-chunking-p stream) nil
                 (chunked-stream-input-chunking-p stream) nil)
           ;; switch back to bare socket stream
           (chunked-stream-stream stream))
          (t stream))))

(defgeneric dispatch-request (acceptor request reply)
  (:documentation "")
  (:method (acceptor request reply)
   (loop for dispatcher in (or (acceptor-dispatch-table acceptor)
                               *dispatch-table*)
         for action = (funcall dispatcher request)
         when action return (funcall action)
         finally (setf (return-code reply) +http-not-found+))))

(defgeneric process-connection (acceptor socket)
  (:documentation "This function is called by the connection dispatcher
when a new client connection has been established.  Arguments are the
ACCEPTOR object and a usocket socket stream object \(or a LispWorks
socket handle) in SOCKET.  It reads the request headers and hands over
to PROCESS-REQUEST.  This is done in a loop until the stream has to be
closed or until a connection timeout occurs.")
  (:method :around (*acceptor* socket)
    "The around method does the error handling."
    (declare (ignore socket))
    ;; note that this call also binds *ACCEPTOR*
    (handler-bind ((error
                    ;; abort if there's an error which isn't caught inside
                    (lambda (cond)
                      (log-message* *lisp-errors-log-level*
                                    "Error while processing connection: ~A" cond)                    
                      (return-from process-connection)))
                   (warning
                    ;; log all warnings which aren't caught inside
                    (lambda (cond)
                      (log-message* *lisp-warnings-log-level*
                                    "Warning while processing connection: ~A" cond))))
      (call-next-method)))
  (:method (*acceptor* socket)
   (let ((*hunchentoot-stream*
          (initialize-connection-stream *acceptor* (make-socket-stream socket *acceptor*))))
      (unwind-protect
          ;; process requests until either the acceptor is shut down,
          ;; *CLOSE-HUNCHENTOOT-STREAM* has been set to T by the
          ;; handler or the peer fails to send a request.
          (do ((*close-hunchentoot-stream* t))
              ((acceptor-shutdown-p *acceptor*))
            (multiple-value-bind (headers-in method url-string acceptor-protocol)
                (get-request-data *hunchentoot-stream*)
              ;; check if there was a request at all
              (unless method
                (return))
              ;; bind per-request special variables, then process the
              ;; request - note that *ACCEPTOR* was bound above already
              (let ((*reply* (make-instance 'reply))
                    (*session* nil))
                (when (acceptor-input-chunking-p *acceptor*)
                  (let ((transfer-encodings (cdr (assoc* :transfer-encoding headers-in))))
                    (when transfer-encodings
                      (setq transfer-encodings
                            (split "\\s*,\\*" transfer-encodings)))
                    (when (member "chunked" transfer-encodings :test #'equalp)
                      ;; turn chunking on before we read the request body
                      (setf *hunchentoot-stream* (make-chunked-stream *hunchentoot-stream*)
                            (chunked-stream-input-chunking-p *hunchentoot-stream*) t))))
                (multiple-value-bind (remote-addr remote-port)
                    (get-peer-address-and-port socket)
                  (process-request (make-instance (acceptor-request-class *acceptor*)
                                                  :remote-addr remote-addr
                                                  :remote-port remote-port
                                                  :headers-in headers-in
                                                  :content-stream *hunchentoot-stream*
                                                  :method method
                                                  :uri url-string
                                                  :server-protocol acceptor-protocol))))
              (force-output *hunchentoot-stream*)
              (setq *hunchentoot-stream* (reset-connection-stream *acceptor* *hunchentoot-stream*))
              (when *close-hunchentoot-stream*
                (return))))
        (when *hunchentoot-stream*
          ;; as we are at the end of the request here, we ignore all
          ;; errors that may occur while flushing and/or closing the
          ;; stream.
          (ignore-errors
            (force-output *hunchentoot-stream*)
            (close *hunchentoot-stream* :abort t)))))))

(defun process-request (request)
  "This function is called by PROCESS-CONNECTION after the incoming
headers have been read.  It sets up the REQUEST and REPLY objects,
dispatches to a handler, and finally sends the output to the client
using START-OUTPUT.  If all goes as planned, the function returns T."
  (let (*tmp-files* *headers-sent*)
    (unwind-protect
        (let* ((*request* request))
          (multiple-value-bind (body error)
              (catch 'handler-done
                (handler-bind ((error
                                (lambda (cond)
                                  (when *log-lisp-errors-p*
                                    (log-message* *lisp-errors-log-level* "~A" cond))
                                  ;; if the headers were already sent
                                  ;; the error happens within the body
                                  ;; and we have to close the stream
                                  (when *headers-sent*
                                    (setq *close-hunchentoot-stream* t))
                                  (throw 'handler-done
                                         (values nil cond))))
                               (warning
                                (lambda (cond)
                                  (when *log-lisp-warnings-p*
                                    (log-message* *lisp-warnings-log-level* "~A" cond)))))
                  ;; skip dispatch if bad request
                  (when (eql (return-code) +http-ok+)
                    ;; now do the work
                    (dispatch-request *acceptor* *request* *reply*))))
            (when error
              (setf (return-code *reply*)
                    +http-internal-server-error+))
            (start-output :content (cond (error
                                          "An error has occured.")
                                         (t body))))
          t)
      (dolist (path *tmp-files*)
        (when (and (pathnamep path) (probe-file path))
          ;; the handler may have chosen to (re)move the uploaded
          ;; file, so ignore errors that happen during deletion.
          (ignore-errors
            (delete-file path)))))))
