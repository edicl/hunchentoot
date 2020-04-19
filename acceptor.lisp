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

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun default-document-directory (&optional sub-directory)
    (let ((source-directory #.(or *compile-file-truename* *load-truename*)))
      (merge-pathnames (make-pathname :directory (append (pathname-directory source-directory)
                                                         (list "www")
                                                         (when sub-directory
                                                           (list sub-directory)))
                                      :name nil
                                      :type nil
                                      :defaults source-directory)))))

(defclass acceptor ()
  ((port :initarg :port
         :reader acceptor-port
         :documentation "The port the acceptor is listening on.  The
default is 80.  Note that depending on your operating system you might
need special privileges to listen on port 80.  When 0, the port will be
chosen by the system the first time the acceptor is started.")
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
   (listen-socket :initform nil
                  :accessor acceptor-listen-socket
                  :documentation "The socket listening for incoming
connections.")
   #-(or :lispworks4 :lispworks5 :lispworks6)
   (listen-backlog :initarg :listen-backlog
                   :reader acceptor-listen-backlog
                   :documentation "Number of pending connections
          allowed in the listen socket before the kernel rejects
          further incoming connections.")
   (acceptor-shutdown-p :initform t
                        :accessor acceptor-shutdown-p
                        :documentation "A flag that makes the acceptor
shutdown itself when set to something other than NIL.")
   (requests-in-progress :initform 0
                         :accessor acceptor-requests-in-progress
                         :documentation "The number of
requests currently in progress.")
   (shutdown-queue :initform (make-condition-variable)
                   :accessor acceptor-shutdown-queue
                   :documentation "A condition variable
used with soft shutdown, signaled when all requests
have been processed.")
   (shutdown-lock :initform (make-lock "hunchentoot-acceptor-shutdown")
                  :accessor acceptor-shutdown-lock
                  :documentation "The lock protecting the shutdown-queue
condition variable and the requests-in-progress counter.")
   (access-log-destination :initarg :access-log-destination
                        :accessor acceptor-access-log-destination
                        :documentation "Destination of the access log
which contains one log entry per request handled in a format similar
to Apache's access.log.  Can be set to a pathname or string
designating the log file, to a open output stream or to NIL to
suppress logging.")
   (message-log-destination :initarg :message-log-destination
                         :accessor acceptor-message-log-destination
                         :documentation "Destination of the server
error log which is used to log informational, warning and error
messages in a free-text format intended for human inspection. Can be
set to a pathname or string designating the log file, to a open output
stream or to NIL to suppress logging.")
   (error-template-directory :initarg :error-template-directory
                             :accessor acceptor-error-template-directory
                             :documentation "Directory pathname that
 contains error message template files for server-generated error
 messages.  Files must be named <return-code>.html with <return-code>
 representing the HTTP return code that the file applies to,
 i.e. 404.html would be used as the content for a HTTP 404 Not found
 response.")
   (document-root :initarg :document-root
                  :accessor acceptor-document-root
                  :documentation "Directory pathname that points to
files that are served by the acceptor if no more specific
acceptor-dispatch-request method handles the request."))
  (:default-initargs
   :address nil
   :port 80
   :name (gensym)
   :request-class 'request
   :reply-class 'reply
   #-(or :lispworks4 :lispworks5 :lispworks6) :listen-backlog
   #-(or :lispworks4 :lispworks5 :lispworks6) 50
   :taskmaster (make-instance (cond (*supports-threads-p* 'one-thread-per-connection-taskmaster)
                                    (t 'single-threaded-taskmaster)))
   :output-chunking-p t
   :input-chunking-p t
   :persistent-connections-p t
   :read-timeout *default-connection-timeout*
   :write-timeout *default-connection-timeout*
   :access-log-destination *error-output*
   :message-log-destination *error-output*
   :document-root (load-time-value (default-document-directory))
   :error-template-directory (load-time-value (default-document-directory "errors")))
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

(defmethod initialize-instance :after ((acceptor acceptor) &key)
  (with-accessors ((document-root acceptor-document-root)
                   (persistent-connections-p acceptor-persistent-connections-p)
                   (taskmaster acceptor-taskmaster)
                   (error-template-directory acceptor-error-template-directory)) acceptor
    (when (typep taskmaster
                 'single-threaded-taskmaster)
      (setf persistent-connections-p nil))
    (when document-root
      (setf document-root (translate-logical-pathname document-root)))
    (when error-template-directory
      (setf error-template-directory (translate-logical-pathname error-template-directory)))))

(defgeneric start (acceptor)
  (:documentation "Starts the ACCEPTOR so that it begins accepting
connections.  Returns the acceptor."))

(defgeneric stop (acceptor &key soft)
  (:documentation "Stops the ACCEPTOR so that it no longer accepts
requests.  If SOFT is true, and there are any requests in progress,
wait until all requests are fully processed, but meanwhile do not
accept new requests.  Note that SOFT must not be set when calling
STOP from within a request handler, as that will deadlock."))

(defgeneric started-p (acceptor)
  (:documentation "Tells if ACCEPTOR has been started.
The default implementation simply queries ACCEPTOR for its listening
status, so if T is returned to the calling thread, then some thread
has called START or some thread's call to STOP hasn't finished. If NIL
is returned either some thread has called STOP, or some thread's call
to START hasn't finished or START was never called at all for
ACCEPTOR.")
  (:method (acceptor)
    #-lispworks (and (acceptor-listen-socket acceptor) t)
    #+lispworks (not (acceptor-shutdown-p acceptor))))

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
immediately, on other Lisps it returns only once the acceptor has been
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
really, really know what you're doing."))

(defgeneric handle-request (acceptor request)
  (:documentation "This function is called once the request has been
read and a REQUEST object has been created.  Its job is to set up
standard error handling and request logging.

Might be a good place for around methods specialized for your subclass
of ACCEPTOR which bind or rebind special variables which can then be
accessed by your handlers."))

(defgeneric acceptor-dispatch-request (acceptor request)
  (:documentation "This function is called to actually dispatch the
request once the standard logging and error handling has been set up.
ACCEPTOR subclasses implement methods for this function in order to
perform their own request routing.  If a method does not want to
handle the request, it is supposed to invoke CALL-NEXT-METHOD so that
the next ACCEPTOR in the inheritance chain gets a chance to handle the
request."))

(defgeneric acceptor-ssl-p (acceptor)
  (:documentation "Returns a true value if ACCEPTOR uses SSL
connections.  The default is to unconditionally return NIL and
subclasses of ACCEPTOR must specialize this method to signal that
they're using secure connections - see the SSL-ACCEPTOR class."))

;; general implementation

(defmethod start ((acceptor acceptor))
  (setf (acceptor-shutdown-p acceptor) nil)
  (let ((taskmaster (acceptor-taskmaster acceptor)))
    (setf (taskmaster-acceptor taskmaster) acceptor)
    (start-listening acceptor)
    (execute-acceptor taskmaster))
  acceptor)

(defmethod stop ((acceptor acceptor) &key soft)
  (with-lock-held ((acceptor-shutdown-lock acceptor))
    (setf (acceptor-shutdown-p acceptor) t))
  #-lispworks
  (wake-acceptor-for-shutdown acceptor)
  (when soft
    (with-lock-held ((acceptor-shutdown-lock acceptor))
      (when (plusp (acceptor-requests-in-progress acceptor))
        (condition-variable-wait (acceptor-shutdown-queue acceptor)
                                 (acceptor-shutdown-lock acceptor)))))
  (shutdown (acceptor-taskmaster acceptor))
  #-lispworks
  (usocket:socket-close (acceptor-listen-socket acceptor))
  #-lispworks
  (setf (acceptor-listen-socket acceptor) nil)
  #+lispworks
  (mp:process-kill (acceptor-process acceptor))
  acceptor)

#-lispworks
(defun wake-acceptor-for-shutdown (acceptor)
  "Creates a dummy connection to the acceptor, waking ACCEPT-CONNECTIONS while it is waiting.
This is supposed to force a check of ACCEPTOR-SHUTDOWN-P."
  (handler-case
      (multiple-value-bind (address port) (usocket:get-local-name (acceptor-listen-socket acceptor))
        (let ((conn (usocket:socket-connect address port)))
          (usocket:socket-close conn)))
    (error (e)
      (acceptor-log-message acceptor :error "Wake-for-shutdown connect failed: ~A" e))))

(defmethod initialize-connection-stream ((acceptor acceptor) stream)
 ;; default method does nothing
 stream)

(defmethod reset-connection-stream ((acceptor acceptor) stream)
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
  ;; note that this method also binds *ACCEPTOR*
  (with-conditions-caught-and-logged ()
    (with-mapped-conditions ()
      (call-next-method))))

(defun do-with-acceptor-request-count-incremented (*acceptor* function)
  (with-lock-held ((acceptor-shutdown-lock *acceptor*))
    (incf (acceptor-requests-in-progress *acceptor*)))
  (unwind-protect
       (funcall function)
    (with-lock-held ((acceptor-shutdown-lock *acceptor*))
      (decf (acceptor-requests-in-progress *acceptor*))
      (when (acceptor-shutdown-p *acceptor*)
        (condition-variable-signal (acceptor-shutdown-queue *acceptor*))))))

(defmacro with-acceptor-request-count-incremented ((acceptor) &body body)
  "Execute BODY with ACCEPTOR-REQUESTS-IN-PROGRESS of ACCEPTOR
  incremented by one.  If the ACCEPTOR-SHUTDOWN-P returns true after
  the BODY has been executed, the ACCEPTOR-SHUTDOWN-QUEUE condition
  variable of the ACCEPTOR is signalled in order to finish shutdown
  processing."
  `(do-with-acceptor-request-count-incremented ,acceptor (lambda () ,@body)))

(defun acceptor-make-request (acceptor socket
                              &key
                                  headers-in
                                  content-stream
                                  method
                                  uri
                                  remote
                                  local
                                  server-protocol)
  "Make a REQUEST instance for the ACCEPTOR, setting up those slots
  that are determined from the SOCKET by calling the appropriate
  socket query functions."
  (multiple-value-bind (remote-addr remote-port)
      (if remote
          (values-list remote)
          (get-peer-address-and-port socket))
    (multiple-value-bind (local-addr local-port)
        (if local
            (values-list local)
            (get-local-address-and-port socket))
      (make-instance (acceptor-request-class acceptor)
                     :acceptor acceptor
                     :local-addr local-addr
                     :local-port local-port
                     :remote-addr remote-addr
                     :remote-port remote-port
                     :headers-in headers-in
                     :content-stream content-stream
                     :method method
                     :uri uri
                     :server-protocol server-protocol))))

(defgeneric detach-socket (acceptor)
  (:documentation "Indicate to Hunchentoot that it should stop serving
                   requests on the current request's socket.
                   Hunchentoot will finish processing the current
                   request and then return from PROCESS-CONNECTION
                   without closing the connection to the client.
                   DETACH-SOCKET can only be called from within a
                   request handler function."))

(defmethod detach-socket ((acceptor acceptor))
  (setf *finish-processing-socket* t
        *close-hunchentoot-stream* nil))

(defmethod process-connection ((*acceptor* acceptor) (socket t))
  (let* ((socket-stream (make-socket-stream socket *acceptor*))
         (*hunchentoot-stream*)
         (*close-hunchentoot-stream* t)
         (remote (multiple-value-list (get-peer-address-and-port socket)))
         (local (multiple-value-list (get-local-address-and-port socket))))
    (unwind-protect
         ;; process requests until either the acceptor is shut down,
         ;; *CLOSE-HUNCHENTOOT-STREAM* has been set to T by the
         ;; handler, or the peer fails to send a request
         (progn
           (setq *hunchentoot-stream* (initialize-connection-stream *acceptor* socket-stream))
           (loop
              (let ((*finish-processing-socket* t))
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
                            (split "\\s*,\\s*" transfer-encodings))
                      (when (member "chunked" transfer-encodings :test #'equalp)
                        (cond ((acceptor-input-chunking-p *acceptor*)
                               ;; turn chunking on before we read the request body
                               (setf *hunchentoot-stream* (make-chunked-stream *hunchentoot-stream*)
                                     (chunked-stream-input-chunking-p *hunchentoot-stream*) t))
                              (t (hunchentoot-error "Client tried to use ~
chunked encoding, but acceptor is configured to not use it.")))))
                    (with-acceptor-request-count-incremented (*acceptor*)
                      (process-request (acceptor-make-request *acceptor* socket
                                                              :headers-in headers-in
                                                              :content-stream *hunchentoot-stream*
                                                              :method method
                                                              :uri url-string
                                                              :remote remote
                                                              :local local
                                                              :server-protocol protocol))))
                  (finish-output *hunchentoot-stream*)
                  (setq *hunchentoot-stream* (reset-connection-stream *acceptor* *hunchentoot-stream*))
                  (when *finish-processing-socket*
                    (return))))))
      (when *close-hunchentoot-stream*
        (flet ((close-stream (stream)
                 ;; as we are at the end of the request here, we ignore all
                 ;; errors that may occur while flushing and/or closing the
                 ;; stream.
                 (ignore-errors
                  (finish-output stream))
                 (ignore-errors
                  (close stream :abort t))))
          (unless (or (not *hunchentoot-stream*)
                      (eql socket-stream *hunchentoot-stream*))
            (close-stream *hunchentoot-stream*))
          (close-stream socket-stream))))))

(defmethod acceptor-ssl-p ((acceptor t))
  ;; the default is to always answer "no"
  nil)

(defgeneric acceptor-log-access (acceptor &key return-code)
  (:documentation
   "Function to call to log access to the acceptor.  The RETURN-CODE,
CONTENT and CONTENT-LENGTH keyword arguments contain additional
information about the request to log.  In addition, it can use the
standard request accessor functions that are available to handler
functions to find out more information about the request."))

(defmethod acceptor-log-access ((acceptor acceptor) &key return-code)
  "Default method for access logging.  It logs the information to the
destination determined by (ACCEPTOR-ACCESS-LOG-DESTINATION ACCEPTOR)
\(unless that value is NIL) in a format that can be parsed by most
Apache log analysis tools.)"

  (with-log-stream (stream (acceptor-access-log-destination acceptor) *access-log-lock*)
    (format stream "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] [~A] \"~A ~A~@[?~A~] ~
                    ~A\" ~D ~:[-~;~:*~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\"~%"
            (remote-addr*)
            (header-in* :x-forwarded-for)
            (authorization)
            (iso-time)
            (request-method*)
            (script-name*)
            (query-string*)
            (server-protocol*)
            return-code
            (content-length*)
            (referer)
            (user-agent))))

(defgeneric acceptor-log-message (acceptor log-level format-string &rest format-arguments)
  (:documentation
   "Function to call to log messages by the ACCEPTOR.  It must accept
a severity level for the message, which will be one of :ERROR, :INFO,
or :WARNING, a format string and an arbitary number of formatting
arguments."))

(defmethod acceptor-log-message ((acceptor acceptor) log-level format-string &rest format-arguments)
  "Default function to log server messages.  Sends a formatted message
  to the destination denoted by (ACCEPTOR-MESSAGE-LOG-DESTINATION
  ACCEPTOR).  FORMAT and ARGS are as in FORMAT.  LOG-LEVEL is a
  keyword denoting the log level or NIL in which case it is ignored."
  (with-log-stream (stream (acceptor-message-log-destination acceptor) *message-log-lock*)
    (handler-case
        (format stream "[~A~@[ [~A]~]] ~?~%"
                (iso-time) log-level
                format-string format-arguments)
      (error (e)
        (ignore-errors
         (format *trace-output* "error ~A while writing to error log, error not logged~%" e))))))

(defun log-message* (log-level format-string &rest format-arguments)
  "Convenience function which calls the message logger of the current
acceptor \(if there is one) with the same arguments it accepts.

This is the function which Hunchentoot itself uses to log errors it
catches during request processing."
  (apply 'acceptor-log-message *acceptor* log-level format-string format-arguments))

;; usocket implementation

#-:lispworks
(defmethod start-listening ((acceptor acceptor))
  (when (acceptor-listen-socket acceptor)
    (hunchentoot-error "acceptor ~A is already listening" acceptor))
  (setf (acceptor-listen-socket acceptor)
        (usocket:socket-listen (or (acceptor-address acceptor)
                                   usocket:*wildcard-host*)
                               (acceptor-port acceptor)
                               :reuseaddress t
                               :backlog (acceptor-listen-backlog acceptor)
                               :element-type '(unsigned-byte 8)))
  (values))

#-:lispworks
(defmethod start-listening :after ((acceptor acceptor))
  (when (zerop (acceptor-port acceptor))
    (setf (slot-value acceptor 'port) (usocket:get-local-port (acceptor-listen-socket acceptor)))))

#-:lispworks
(defmethod accept-connections ((acceptor acceptor))
  (usocket:with-server-socket (listener (acceptor-listen-socket acceptor))
    (loop
      (with-lock-held ((acceptor-shutdown-lock acceptor))
        (when (acceptor-shutdown-p acceptor)
          (return)))
      (when (usocket:wait-for-input listener :ready-only t)
       (when-let (client-connection
                  (handler-case (usocket:socket-accept listener)
                    ;; ignore condition
                    (usocket:connection-aborted-error ())))
         (set-timeouts client-connection
                       (acceptor-read-timeout acceptor)
                       (acceptor-write-timeout acceptor))
         (handle-incoming-connection (acceptor-taskmaster acceptor)
                                     client-connection))))))

;; LispWorks implementation

#+:lispworks
(defmethod start-listening ((acceptor acceptor))
  (multiple-value-bind (listener-process startup-condition)
      (comm:start-up-server :service (acceptor-port acceptor)
                            :address (acceptor-address acceptor)
                            :process-name (format nil "Hunchentoot listener \(~A:~A)"
                                                  (or (acceptor-address acceptor) "*")
                                                  (acceptor-port acceptor))
                            #-(or :lispworks4 :lispworks5 :lispworks6)
                            :backlog
                            #-(or :lispworks4 :lispworks5 :lispworks6)
                            (acceptor-listen-backlog acceptor)
                            ;; this function is called once on startup - we
                            ;; use it to check for errors and random port
                            :announce (lambda (socket &optional condition)
                                        (when condition
                                          (error condition))
                                        (when (or (null (acceptor-port acceptor))
                                                  (zerop (acceptor-port acceptor)))
                                          (multiple-value-bind (address port)
                                              (comm:get-socket-address socket)
                                            (declare (ignore address))
                                            (setf (slot-value acceptor 'port) port))))
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

(defmethod acceptor-dispatch-request ((acceptor acceptor) request)
  "Detault implementation of the request dispatch method, generates an
+http-not-found+ error."
  (let ((path (and (acceptor-document-root acceptor)
                   (request-pathname request))))
    (cond
      (path
       (handle-static-file
        (merge-pathnames (if (equal "/" (script-name request)) #p"index.html" path)
                         (acceptor-document-root acceptor))))
      (t
       (setf (return-code *reply*) +http-not-found+)
       (abort-request-handler)))))

(defmethod handle-request ((*acceptor* acceptor) (*request* request))
  "Standard method for request handling.  Calls the request dispatcher
of *ACCEPTOR* to determine how the request should be handled.  Also
sets up standard error handling which catches any errors within the
handler."
  (handler-bind ((error
                  (lambda (cond)
                    ;; if the headers were already sent, the error
                    ;; happened within the body and we have to close
                    ;; the stream
                    (when *headers-sent*
                      (setq *finish-processing-socket* t))
                    (throw 'handler-done
                      (values nil cond (get-backtrace))))))
    (with-debugger
      (acceptor-dispatch-request *acceptor* *request*))))

(defgeneric acceptor-status-message (acceptor http-status-code &key &allow-other-keys)
  (:documentation
   "This function is called after the request's handler has been
   invoked to convert the HTTP-STATUS-CODE to a HTML message to be
   displayed to the user.  If this function returns a string, that
   string is sent to the client instead of the content produced by the
   handler, if any.

   If an ERROR-TEMPLATE-DIRECTORY is set in the current acceptor and
   the directory contains a file corresponding to HTTP-STATUS-CODE
   named <code>.html, that file is sent to the client after variable
   substitution.  Variables are referenced by ${<variable-name>}.

   Additional keyword arguments may be provided which are made
   available to the templating logic as substitution variables.  These
   variables can be interpolated into error message templates in,
   which contains the current URL relative to the server and without
   GET parameters.

   In addition to the variables corresponding to keyword arguments,
   the script-name, lisp-implementation-type,
   lisp-implementation-version and hunchentoot-version variables are
   available."))

(defun make-cooked-message (http-status-code &key error backtrace)
  (labels ((cooked-message (format &rest arguments)
             (setf (content-type*) "text/html; charset=iso-8859-1")
             (format nil "<html><head><title>~D ~A</title></head><body><h1>~:*~A</h1>~?<p><hr>~A</p></body></html>"
                     http-status-code (reason-phrase http-status-code)
                     format (mapcar (lambda (arg)
                                      (if (stringp arg)
                                          (escape-for-html arg)
                                          arg))
                                    arguments)
                     (address-string))))
    (case http-status-code
      ((#.+http-moved-temporarily+
        #.+http-moved-permanently+)
       (cooked-message "The document has moved <a href='~A'>here</a>" (header-out :location)))
      ((#.+http-authorization-required+)
       (cooked-message "The server could not verify that you are authorized to access the document requested.  ~
                        Either you supplied the wrong credentials \(e.g., bad password), or your browser doesn't ~
                        understand how to supply the credentials required."))
      ((#.+http-forbidden+)
       (cooked-message "You don't have permission to access ~A on this server."
                       (script-name *request*)))
      ((#.+http-not-found+)
       (cooked-message "The requested URL ~A was not found on this server."
                       (script-name *request*)))
      ((#.+http-bad-request+)
       (cooked-message "Your browser sent a request that this server could not understand."))
      ((#.+http-internal-server-error+)
       (if *show-lisp-errors-p*
           (cooked-message "<pre>~A~@[~%~%Backtrace:~%~%~A~]</pre>"
                           (escape-for-html (princ-to-string error))
                           (when *show-lisp-backtraces-p*
                             (escape-for-html (princ-to-string backtrace))))
           (cooked-message "An error has occurred")))
      (t
         (when (<= 400 http-status-code)
           (cooked-message "An error has occurred"))))))

(defmethod acceptor-status-message ((acceptor t) http-status-code &rest args &key &allow-other-keys)
  (apply 'make-cooked-message http-status-code args))

(defmethod acceptor-status-message :around ((acceptor acceptor) http-status-code &rest args &key &allow-other-keys)
  (handler-case
      (call-next-method)
    (error (e)
      (log-message* :error "error ~A during error processing, sending cooked message to client" e)
      (apply 'make-cooked-message http-status-code args))))

(defun string-as-keyword (string)
  "Intern STRING as keyword using the reader so that case conversion is done with the reader defaults."
  (let ((*package* (find-package :keyword)))
    (read-from-string string)))

(defmethod acceptor-status-message ((acceptor acceptor) http-status-code &rest properties &key &allow-other-keys)
  "Default function to generate error message sent to the client."
  (labels
      ((substitute-request-context-variables (string)
         (let ((properties (append `(:script-name ,(script-name*)
                                     :lisp-implementation-type ,(lisp-implementation-type)
                                     :lisp-implementation-version ,(lisp-implementation-version)
                                     :hunchentoot-version ,*hunchentoot-version*)
                                   properties)))
           (unless *show-lisp-backtraces-p*
             (setf (getf properties :backtrace) nil))
           (cl-ppcre:regex-replace-all "(?i)\\$\\{([a-z0-9-_]+)\\}"
                                       string
                                       (lambda (target-string start end match-start match-end reg-starts reg-ends)
                                         (declare (ignore start end match-start match-end))
                                         (let ((variable-name (string-as-keyword (subseq target-string
                                                                                         (aref reg-starts 0)
                                                                                         (aref reg-ends 0)))))
                                           (escape-for-html (princ-to-string (getf properties variable-name variable-name))))))))
       (file-contents (file)
         (let ((buf (make-string (file-length file))))
           (read-sequence buf file)
           buf))
       (error-contents-from-template ()
         (let ((error-file-template-pathname (and (acceptor-error-template-directory acceptor)
                                                  (probe-file (make-pathname :name (princ-to-string http-status-code)
                                                                             :type "html"
                                                                             :defaults (acceptor-error-template-directory acceptor))))))
           (when error-file-template-pathname
             (with-open-file (file error-file-template-pathname :if-does-not-exist nil :element-type 'character)
               (when file
                 (setf (content-type*) "text/html")
                 (substitute-request-context-variables (file-contents file))))))))
    (or (unless (< 300 http-status-code)
          (call-next-method))              ; don't ever try template for positive return codes
        (when *show-lisp-errors-p*
          (error-contents-from-template))  ; try template
        (call-next-method))))              ; fall back to cooked message

(defgeneric acceptor-remove-session (acceptor session)
  (:documentation
   "This function is called whenever a session in ACCEPTOR is being
   destroyed because of a session timout or an explicit REMOVE-SESSION
   call."))

(defmethod acceptor-remove-session ((acceptor acceptor) (session t))
  "Default implementation for the session removal hook function.  This
function is called whenever a session is destroyed."
  nil)

(defgeneric acceptor-server-name (acceptor)
  (:documentation "Returns a string which can be used for 'Server' headers.")
  (:method ((acceptor acceptor))
    (format nil "Hunchentoot ~A" *hunchentoot-version*)))
