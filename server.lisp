;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/server.lisp,v 1.43 2008/04/09 08:17:48 edi Exp $

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

(defclass server ()
  ((port :initarg :port
         :documentation "The port the server is listening on.
See START-SERVER.")
   (address :initarg :address
            :documentation "The address the server is listening
on.  See START-SERVER.")
   (name :initarg :name
         :accessor server-name
         :documentation "The optional name of the server, a symbol.")
   (request-class :initarg :request-class
                  :reader server-request-class
                  :documentation "Determines which class of request
objects is created when a request comes in and should be \(a symbol
naming) a class which inherits from REQUEST.")
   (dispatch-table :initarg :dispatch-table
                   :accessor server-dispatch-table
                   :documentation "The dispatch-table used by this
server.  Can be NIL to denote that *DISPATCH-TABLE* should be used.")
   (output-chunking-p :initarg :output-chunking-p
                      :reader server-output-chunking-p
                      :documentation "Whether the server may use output chunking.")
   (input-chunking-p :initarg :input-chunking-p
                     :reader server-input-chunking-p
                     :documentation "Whether the server may use input chunking.")
   (persistent-connections-p :initarg :persistent-connections-p
                             :accessor server-persistent-connections-p
                             :documentation "Whether the server
supports persistent connections, which is the default for threaded
servers.  If this property is false, Hunchentoot closes incoming
connections after having processed one request.  This is the default
for non-threaded servers.")
   (read-timeout :initarg :read-timeout
                 :reader server-read-timeout
                 :documentation "The connection timeout of the server,
specified in (fractional) seconds.  Connections that are idle for
longer than this time are closed by Hunchentoot.  The precise
semantics of this parameter is determined by the underlying Lisp's
implementation of socket timeouts.")
   (write-timeout :initarg :write-timeout
                 :reader server-write-timeout
                 :documentation "The connection timeout of the server,
specified in (fractional) seconds.  The precise semantics of this
parameter is determined by the underlying Lisp's implementation of
socket timeouts.")
   (connection-manager :initarg :connection-manager
                       :initform nil
                       :reader server-connection-manager
                       :documentation "The connection manager that is
responsible for listening to new connections and scheduling them for
execution.")
   #+:lispworks
   (acceptor :accessor server-acceptor
             :documentation "The Lisp process which accepts incoming
             requests.")
   #-:lispworks
   (listen-socket :accessor server-listen-socket
                  :documentation "The listen socket for incoming
                  connections.")
   (server-shutdown-p :initform nil
                      :accessor server-shutdown-p
                      :documentation "Flag that makes the server
shutdown itself when set to something other than NIL.")
   (access-logger :initarg :access-logger
                  :accessor server-access-logger
                  :documentation "Designator for a function to call to
log access to the server.  The function must accept the RETURN-CODE,
CONTENT and CONTENT-LENGTH keyword arguments which are used to pass in
additional information about the request to log.  In addition, it can
use the standard request accessor functions that are available to
handler functions to find out more information about the request.
This slot defaults to the LOG-ACCESS function which logs the
information to a file in a format that can be parsed by most Apache
log analysis tools.")
   (message-logger :initarg :message-logger
                   :accessor server-message-logger
                   :documentation "Designator for a function to call
to log messages by the server.  It must accept a severity level for
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
information about a running Hunchentoot server instance."))

(defmethod initialize-instance :after ((server server)
                                       &key connection-manager-class
                                            connection-manager-arguments
                                            (threaded *supports-threads-p* threaded-specified-p)
                                            (persistent-connections-p
                                             threaded
                                             persistent-connections-specified-p)
                                            (connection-timeout
                                             *default-connection-timeout*
                                             connection-timeout-provided-p)
                                            (read-timeout nil read-timeout-provided-p)
                                            (write-timeout nil write-timeout-provided-p))
  "The CONNECTION-MANAGER-CLASS and CONNECTION-MANAGER-ARGUMENTS
arguments to the creation of a server instance determine the
connection manager instance that is created.  THREADED is the user
friendly version of the CONNECTION-MANAGER-CLASS option.  If it is
NIL, an unthreaded connection manager is used.  It is an error to
specify both THREADED and a CONNECTION-MANAGER-CLASS argument.

The PERSISTENT-CONNECTIONS-P keyword argument defaults to the value of
the THREADED keyword argument but can be overridden.

If a neither READ-TIMEOUT nor WRITE-TIMEOUT are specified by the user,
the server's read and write timeouts default to the CONNECTION-TIMEOUT
value.  If either of READ-TIMEOUT or WRITE-TIMEOUT is specified,
CONNECTION-TIMEOUT is not used and may not be supplied."
  (declare (ignore read-timeout write-timeout))
  (when (and threaded-specified-p connection-manager-class)
    (parameter-error "Can't use both THREADED and CONNECTION-MANAGER-CLASS arguments."))
  (unless persistent-connections-specified-p
    (setf (server-persistent-connections-p server) persistent-connections-p))
  (unless (server-connection-manager server)
    (setf (slot-value server 'connection-manager)
          (apply #'make-instance
                 (or connection-manager-class
                     (if threaded
                         'one-thread-per-connection-manager
                         'single-threaded-connection-manager))
                 :server server
                 connection-manager-arguments)))
  (if (or read-timeout-provided-p write-timeout-provided-p)
      (when connection-timeout-provided-p
        (parameter-error "Can't have both CONNECTION-TIMEOUT and either of READ-TIMEOUT and WRITE-TIMEOUT."))
      (setf (slot-value server 'read-timeout) connection-timeout
            (slot-value server 'write-timeout) connection-timeout)))

(defgeneric server-ssl-p (server)
  (:documentation "Returns a true value if SERVER is an SSL server.")
  (:method ((server t))
    nil))

(defun ssl-p (&optional (server *server*))
  (server-ssl-p server))

(defmethod print-object ((server server) stream)
  (print-unreadable-object (server stream :type t)
    (format stream "\(host ~A, port ~A)"
            (or (server-address server) "*") (server-port server))))

(defun server-address (&optional (server *server*))
  "Returns the address at which the current request arrived."
  (slot-value server 'address))

(defun server-port (&optional (server *server*))
  "Returns the port at which the current request arrived."
  (slot-value server 'port))

(defgeneric start (server)
  (:documentation "Start the SERVER so that it begins accepting
connections.")
  (:method ((server server))
    (start-listening server)
    (execute-acceptor (server-connection-manager server))))

(defgeneric stop (server)
  (:documentation "Stop the SERVER so that it does no longer accept requests.")
  (:method ((server server))
   (setf (server-shutdown-p server) t)
   (shutdown (server-connection-manager server))
   #-:lispworks
   (usocket:socket-close (server-listen-socket server))))

(defun start-server (&rest args
                     &key port address dispatch-table name
                     threaded
                     input-chunking-p connection-timeout
                     persistent-connections-p
                     read-timeout write-timeout
                     #+(and :unix (not :win32)) setuid
                     #+(and :unix (not :win32)) setgid
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

CONNECTION-MANAGER-CLASS specifies the name of the class to instantiate
for managing how connections are mapped to threads.  You don't normally
want to specify this argument unless you want to have non-standard
threading behavior.   See the documentation for more information.

On Unix you can use SETUID and SETGID to change the UID and GID of the
process directly after the server has been started.  \(You might want
to do this if you're using a privileged port like 80.)  SETUID and
SETGID can be integers \(the actual IDs) or strings \(for the user and
group name respectively).

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
                       (if ssl-certificate-file 'ssl-server 'server)
                       #+:hunchentoot-no-ssl
                       'server
                       args)))
    (start server)
    #+(and :unix (not :win32))
    (when setgid
      ;; we must make sure to call setgid before we call setuid or
      ;; suddenly we aren't root anymore...
      (etypecase setgid
        (integer (setgid setgid))
        (string (setgid (get-gid-from-name setgid)))))
    #+(and :unix (not :win32))
    (when setuid
      (etypecase setuid
        (integer (setuid setuid))
        (string (setuid (get-uid-from-name setuid)))))   
    server))

(defun stop-server (server)
  "Stops the Hunchentoot server SERVER."
  (stop server))

;; connection manager API

(defconstant +new-connection-wait-time+ 2
  "Time in seconds to wait for a new connection to arrive before
performing a cleanup run.")

(defgeneric start-listening (server)
  (:documentation "Sets up a listen socket for the given SERVER and
enables it to listen for incoming connections.  This function is
called from the thread that starts the server initially and may return
errors resulting from the listening operation. (like 'address in use'
or similar).")
  (:method ((server server))
    #+:lispworks
    (multiple-value-bind (listener-process startup-condition)
        (comm:start-up-server :service (server-port server)
                                :address (server-address server)
                                :process-name (format nil "Hunchentoot listener \(~A:~A)"
                                                      (or (server-address server) "*") (server-port server))
                                ;; this function is called once on startup - we
                                ;; use it to check for errors
                                :announce (lambda (socket &optional condition)
                                            (declare (ignore socket))
                                            (when condition
                                              (error condition)))
                                ;; this function is called whenever a connection
                                ;; is made
                                :function (lambda (handle)
                                            (unless (server-shutdown-p server)
                                              (handle-incoming-connection
                                               (server-connection-manager server) handle)))
                                ;; wait until the server was successfully started
                                ;; or an error condition is returned
                                :wait t)
      (when startup-condition
        (error startup-condition))
      (process-stop listener-process)
      (setf (server-acceptor server) listener-process))
    #-:lispworks
    (setf (server-listen-socket server)
          (usocket:socket-listen (or (server-address server)
                                     usocket:*wildcard-host*)
                                 (server-port server)
                                 :reuseaddress t
                                 :element-type '(unsigned-byte 8)))))

(defgeneric accept-connections (server)
  (:documentation "In a loop, accepts a connection and
dispatches it to the server's connection manager object for processing
using HANDLE-INCOMING-CONNECTION.")
  (:method ((server server))
    #+:lispworks
    (process-unstop (server-acceptor server))
    #-:lispworks
    (usocket:with-server-socket (listener (server-listen-socket server))
      (do ((new-connection-p (usocket:wait-for-input listener :timeout +new-connection-wait-time+)
                             (usocket:wait-for-input listener :timeout +new-connection-wait-time+)))
          ((server-shutdown-p server))
        (when new-connection-p
          (let ((client-connection (usocket:socket-accept listener)))
            (when client-connection
              (set-timeouts client-connection
                            (server-read-timeout server)
                            (server-write-timeout server))
              (handle-incoming-connection (server-connection-manager server)
                                          client-connection))))))))

(defgeneric initialize-connection-stream (server stream) 
 (:documentation "Wraps the given STREAM with all the additional
stream classes to support the functionality required by SERVER.  The
methods of this generic function must return the stream to use.")
 ;; default method does nothing
 (:method (server stream)
  (declare (ignore server))
  stream))

(defgeneric reset-connection-stream (server stream)
  (:documentation "Resets the given STREAM so that it can be used to
process the next request, SERVER is the server that this stream
belongs to, which determines what to do to reset.  This generic
function is called after a request has been processed and must return
the stream.")
  (:method (server stream)
    (declare (ignore server))
    ;; turn chunking off at this point
    (cond ((typep stream 'chunked-stream)
           ;; flush the stream first and check if there's unread input
           ;; which would be an error
           (setf (chunked-stream-output-chunking-p stream) nil
                 (chunked-stream-input-chunking-p stream) nil)
           ;; switch back to bare socket stream
           (chunked-stream-stream stream))
          (t stream))))

(defgeneric dispatch-request (server request reply)
  (:documentation "")
  (:method (server request reply)
   (loop for dispatcher in (or (server-dispatch-table server)
                               *dispatch-table*)
         for action = (funcall dispatcher request)
         when action return (funcall action)
         finally (setf (return-code reply) +http-not-found+))))

(defgeneric process-connection (server socket)
  (:documentation "This function is called by the connection manager
when a new client connection has been established.  Arguments are the
SERVER object and a usocket socket stream object \(or a LispWorks
socket handle) in SOCKET.  It reads the request headers and hands over
to PROCESS-REQUEST.  This is done in a loop until the stream has to be
closed or until a connection timeout occurs.")
  (:method :around (*server* socket)
    "The around method does the error handling."
    (declare (ignore socket))
    ;; note that this call also binds *SERVER*
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
  (:method (*server* socket)
   (let ((*hunchentoot-stream*
          (initialize-connection-stream *server* (make-socket-stream socket *server*))))
      (unwind-protect
          ;; process requests until either the server is shut down,
          ;; *CLOSE-HUNCHENTOOT-STREAM* has been set to T by the
          ;; handler or the peer fails to send a request.
          (do ((*close-hunchentoot-stream* t))
              ((server-shutdown-p *server*))
            (multiple-value-bind (headers-in method url-string server-protocol)
                (get-request-data *hunchentoot-stream*)
              ;; check if there was a request at all
              (unless method
                (return))
              ;; bind per-request special variables, then process the
              ;; request - note that *SERVER* was bound above already
              (let ((*reply* (make-instance 'reply))
                    (*session* nil))
                (when (server-input-chunking-p *server*)
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
                  (process-request (make-instance (server-request-class *server*)
                                                  :remote-addr remote-addr
                                                  :remote-port remote-port
                                                  :headers-in headers-in
                                                  :content-stream *hunchentoot-stream*
                                                  :method method
                                                  :uri url-string
                                                  :server-protocol server-protocol))))
              (force-output *hunchentoot-stream*)
              (setq *hunchentoot-stream* (reset-connection-stream *server* *hunchentoot-stream*))
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
        (let* ((*request* request)
               backtrace)
          (multiple-value-bind (body error)
              (catch 'handler-done
                (handler-bind ((error
                                (lambda (cond)
                                  ;; only generate backtrace if needed
                                  (setq backtrace
                                        (and (or (and *show-lisp-errors-p*
                                                      *show-lisp-backtraces-p*)
                                                 (and *log-lisp-errors-p*
                                                      *log-lisp-backtraces-p*))
                                             (get-backtrace cond)))
                                  (when *log-lisp-errors-p*
                                    (log-message* *lisp-errors-log-level*
                                                  "~A~:[~*~;~%~A~]"
                                                  cond
                                                  *log-lisp-backtraces-p*
                                                  backtrace))
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
                                    (log-message* *lisp-warnings-log-level*
                                                  "~A~:[~*~;~%~A~]"
                                                  cond
                                                  *log-lisp-backtraces-p*
                                                  backtrace)))))
                  ;; skip dispatch if bad request
                  (when (eql (return-code) +http-ok+)
                    ;; now do the work
                    (dispatch-request *server* *request* *reply*))))
            (when error
              (setf (return-code *reply*)
                    +http-internal-server-error+))
            (start-output :content (cond ((and error *show-lisp-errors-p*)
                                          (format nil "<pre>~A~:[~*~;~%~%~A~]</pre>"
                                                  (escape-for-html (format nil "~A" error))
                                                  *show-lisp-backtraces-p*
                                                  (escape-for-html (format nil "~A" backtrace))))
                                         (error
                                          "An error has occured.")
                                         (t body))))
          t)
      (dolist (path *tmp-files*)
        (when (and (pathnamep path) (probe-file path))
          ;; the handler may have chosen to (re)move the uploaded
          ;; file, so ignore errors that happen during deletion.
          (ignore-errors
            (delete-file path)))))))
