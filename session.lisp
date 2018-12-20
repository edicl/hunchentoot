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

(defgeneric session-db-lock (acceptor &key whole-db-p)
  (:documentation "A function which returns a lock that will be used
to prevent concurrent access to sessions.  The first argument will be
the acceptor that handles the current request, the second argument is
true if the whole \(current) session database is modified.  If it is
NIL, only one existing session in the database is modified.

This function can return NIL which means that sessions or session
databases will be modified without a lock held \(for example for
single-threaded environments).  The default is to always return a
global lock \(ignoring the ACCEPTOR argument) for Lisps that support
threads and NIL otherwise."))

(defmethod session-db-lock ((acceptor t) &key (whole-db-p t))
  (declare (ignore whole-db-p))
  *global-session-db-lock*)

(defmacro with-session-lock-held ((lock) &body body)
  "This is like WITH-LOCK-HELD except that it will accept NIL as a
\"lock\" and just execute BODY in this case."
  (with-gensyms (thunk)
    (once-only (lock)
      `(flet ((,thunk () ,@body))
         (cond (,lock (with-lock-held (,lock) (,thunk)))
               (t (,thunk)))))))

(defgeneric session-db (acceptor)
  (:documentation "Returns the current session database which is an
alist where each car is a session's ID and the cdr is the
corresponding SESSION object itself.  The default is to use a global
list for all acceptors."))

(defmethod session-db ((acceptor t))
  *session-db*)

(defgeneric (setf session-db) (new-value acceptor)
  (:documentation "Modifies the current session database.  See SESSION-DB."))

(defmethod (setf session-db) (new-value (acceptor t))
  (setq *session-db* new-value))

(defgeneric next-session-id (acceptor)
  (:documentation "Returns the next sequential session ID, an integer,
which should be unique per session.  The default method uses a simple
global counter and isn't guarded by a lock.  For a high-performance
production environment you might consider using a more robust
implementation."))

(let ((session-id-counter 0))
  (defmethod next-session-id ((acceptor t))
    (incf session-id-counter)))

(defclass session ()
  ((session-id :initform (next-session-id (request-acceptor *request*))
               :reader session-id
               :type integer
               :documentation "The unique ID \(an INTEGER) of the session.")
   (session-string :reader session-string
                   :documentation "The session string encodes enough
data to safely retrieve this session.  It is sent to the browser as a
cookie value or as a GET parameter.")
   (user-agent :initform (user-agent *request*)
               :reader session-user-agent
               :documentation "The incoming 'User-Agent' header that
was sent when this session was created.")
   (remote-addr :initform (real-remote-addr *request*)
                :reader session-remote-addr
                :documentation "The remote IP address of the client
when this session was started as returned by REAL-REMOTE-ADDR.")
   (session-start :initform (get-universal-time)
                  :reader session-start
                  :documentation "The time this session was started.")
   (last-click :initform (get-universal-time)
               :reader session-last-click
               :documentation "The last time this session was used.")
   (session-data :initarg :session-data
                 :initform nil
                 :reader session-data
                 :documentation "Data associated with this session -
see SESSION-VALUE.")
   (max-time :initarg :max-time
             :initform *session-max-time*
             :accessor session-max-time
             :type fixnum
             :documentation "The time \(in seconds) after which this
session expires if it's not used."))
  (:documentation "SESSION objects are automatically maintained by
Hunchentoot.  They should not be created explicitly with MAKE-INSTANCE
but implicitly with START-SESSION and they should be treated as opaque
objects.

You can ignore Hunchentoot's SESSION objects altogether and implement
your own sessions if you provide corresponding methods for
SESSION-COOKIE-VALUE and SESSION-VERIFY."))

(defun encode-session-string (id user-agent remote-addr start)
  "Creates a uniquely encoded session string based on the values ID,
USER-AGENT, REMOTE-ADDR, and START"
  (unless (boundp '*session-secret*)
    (hunchentoot-warn "Session secret is unbound.  Using Lisp's RANDOM function to initialize it.")
    (reset-session-secret))
  ;; *SESSION-SECRET* is used twice due to known theoretical
  ;; vulnerabilities of MD5 encoding
  (md5-hex (concatenate 'string
			*session-secret*
			(md5-hex (format nil "~A~A~@[~A~]~@[~A~]~A"
                                         *session-secret*
                                         id
                                         (and *use-user-agent-for-sessions*
                                              user-agent)
                                         (and *use-remote-addr-for-sessions*
                                              remote-addr)
                                         start)))))

(defun stringify-session (session)
  "Creates a string representing the SESSION object SESSION. See
ENCODE-SESSION-STRING."
  (encode-session-string (session-id session)
                         (session-user-agent session)
                         (session-remote-addr session)
                         (session-start session)))

(defmethod initialize-instance :after ((session session) &rest init-args)
  "Set SESSION-STRING slot after the session has been initialized."
  (declare (ignore init-args))
  (setf (slot-value session 'session-string) (stringify-session session)))

(defun session-gc ()
  "Removes sessions from the current session database which are too
old - see SESSION-TOO-OLD-P."
  (with-session-lock-held ((session-db-lock *acceptor*))
    (setf (session-db *acceptor*)
          (loop for id-session-pair in (session-db *acceptor*)
                for (nil . session) = id-session-pair
                when (session-too-old-p session)
                do (acceptor-remove-session *acceptor* session)
                else
                collect id-session-pair)))
  (values))

(defun session-value (symbol &optional (session *session*))
  "Returns the value associated with SYMBOL from the session object
SESSION \(the default is the current session) if it exists."
  (when session
    (let ((found (assoc symbol (session-data session) :test #'eq)))
      (values (cdr found) found))))

(defsetf session-value (symbol &optional session)
    (new-value)
  "Sets the value associated with SYMBOL from the session object
SESSION. If there is already a value associated with SYMBOL it will be
replaced. Will automatically start a session if none was supplied and
there's no session for the current request."
  (once-only (symbol)
    (with-gensyms (place %session)
      `(let ((,%session (or ,session (start-session))))
         (with-session-lock-held ((session-db-lock *acceptor* :whole-db-p nil))
           (let* ((,place (assoc ,symbol (session-data ,%session) :test #'eq)))
             (cond
               (,place
                (setf (cdr ,place) ,new-value))
               (t
                (push (cons ,symbol ,new-value)
                      (slot-value ,%session 'session-data))
                ,new-value))))))))

(defun delete-session-value (symbol &optional (session *session*))
  "Removes the value associated with SYMBOL from SESSION if there is
one."
  (when session
    (setf (slot-value session 'session-data)
            (delete symbol (session-data session)
                    :key #'car :test #'eq)))
  (values))

(defgeneric session-cookie-value (session)
  (:documentation "Returns a string which can be used to safely
restore the session SESSION if as session has already been
established.  This is used as the value stored in the session cookie
or in the corresponding GET parameter and verified by SESSION-VERIFY.

A default method is provided and there's no reason to change it unless
you want to use your own session objects."))

(defmethod session-cookie-value ((session session))
  (and session
       (format nil
               "~D:~A"
               (session-id session)
               (session-string session))))

(defgeneric session-cookie-name (acceptor)
  (:documentation "Returns the name \(a string) of the cookie \(or the
GET parameter) which is used to store a session on the client side.
The default is to use the string \"hunchentoot-session\", but you can
specialize this function if you want another name."))

(defmethod session-cookie-name ((acceptor t))
  "hunchentoot-session")

(defgeneric session-created (acceptor new-session)
  (:documentation "This function is called whenever a new session has
been created.  There's a default method which might trigger a session
GC based on the value of *SESSION-GC-FREQUENCY*.

The return value is ignored."))

(let ((global-session-usage-counter 0))
  (defmethod session-created ((acceptor t) (session t))
    "Counts session usage globally and triggers session GC if
necessary."
    (when (and *session-gc-frequency*
               (zerop (mod (incf global-session-usage-counter)
                           *session-gc-frequency*)))
      (session-gc))))

(defun start-session ()
  "Returns the current SESSION object. If there is no current session,
creates one and updates the corresponding data structures. In this
case the function will also send a session cookie to the browser."
  (let ((session (session *request*)))
    (when session
      (return-from start-session session))
    (setf session (make-instance 'session)
          (session *request*) session)
    (with-session-lock-held ((session-db-lock *acceptor*))
      (setf (session-db *acceptor*)
            (acons (session-id session) session (session-db *acceptor*))))
    (set-cookie (session-cookie-name *acceptor*)
                :value (session-cookie-value session)
                :path "/"
                :http-only t)
    (session-created *acceptor* session)
    (setq *session* session)))

(defun remove-session (session)
  "Completely removes the SESSION object SESSION from Hunchentoot's
internal session database."
  (set-cookie (session-cookie-name *acceptor*)
              :value "deleted"
              :path "/"
              :expires 0)
  (with-session-lock-held ((session-db-lock *acceptor*))
    (acceptor-remove-session *acceptor* session)
    (setf (session-db *acceptor*)
          (delete (session-id session) (session-db *acceptor*)
                  :key #'car :test #'=)))
  (values))

(defun session-too-old-p (session)
  "Returns true if the SESSION object SESSION has not been active in
the last \(SESSION-MAX-TIME SESSION) seconds."
  (< (+ (session-last-click session) (session-max-time session))
     (get-universal-time)))

(defun get-stored-session (id)
  "Returns the SESSION object corresponding to the number ID if the
session has not expired. Will remove the session if it has expired but
will not create a new one."
  (let ((session
         (cdr (assoc id (session-db *acceptor*) :test #'=))))
    (when (and session
               (session-too-old-p session))
      (when *reply*
        (log-message* :info "Session with ID ~A too old" id))
      (remove-session session)
      (setq session nil))
    session))

(defun regenerate-session-cookie-value (session)
  "Regenerates the cookie value. This should be used
when a user logs in according to the application to prevent against
session fixation attacks. The cookie value being dependent on ID,
USER-AGENT, REMOTE-ADDR, START, and *SESSION-SECRET*, the only value
we can change is START to regenerate a new value. Since we're
generating a new cookie, it makes sense to have the session being
restarted, in time. That said, because of this fact, calling this
function twice in the same second will regenerate twice the same value."
  (setf (slot-value session 'session-start) (get-universal-time)
        (slot-value session 'session-string) (stringify-session session))
  (set-cookie (session-cookie-name *acceptor*)
              :value (session-cookie-value session)
              :path "/"
              :http-only t))

(defgeneric session-verify (request)
  (:documentation "Tries to get a session identifier from the cookies
\(or alternatively from the GET parameters) sent by the client (see
SESSION-COOKIE-NAME and SESSION-COOKIE-VALUE).  This identifier is
then checked for validity against the REQUEST object REQUEST.  On
success the corresponding session object \(if not too old) is returned
\(and updated).  Otherwise NIL is returned.

A default method is provided and you only need to write your own one
if you want to maintain your own sessions."))

(defmethod session-verify ((request request))
  (let ((session-identifier (or (when-let (session-cookie (cookie-in (session-cookie-name *acceptor*) request))
                                  (url-decode session-cookie))
                                (get-parameter (session-cookie-name *acceptor*) request))))
    (when (and (stringp session-identifier)
               (scan "^\\d+:.+" session-identifier))
      (destructuring-bind (id-string session-string)
          (split ":" session-identifier :limit 2)
        (let* ((id (parse-integer id-string))
               (session (get-stored-session id))
               (user-agent (user-agent request))
               (remote-addr (remote-addr request)))
          (cond
            ((and session
                  (string= session-string
                           (session-string session))
                  (string= session-string
                           (encode-session-string id
                                                  user-agent
                                                  (real-remote-addr request)
                                                  (session-start session))))
             ;; the session key presented by the client is valid
             (setf (slot-value session 'last-click) (get-universal-time))
             session)
            (session
             ;; the session ID pointed to an existing session, but the
             ;; session string did not match the expected session string
             (log-message* :warning
                           "Fake session identifier '~A' (User-Agent: '~A', IP: '~A')"
                           session-identifier user-agent remote-addr)
             ;; remove the session to make sure that it can't be used
             ;; again; the original legitimate user will be required to
             ;; log in again
             (remove-session session)
             nil)
            (t
             ;; no session was found under the ID given, presumably
             ;; because it has expired.
             (log-message* :info
                           "No session for session identifier '~A' (User-Agent: '~A', IP: '~A')"
                           session-identifier user-agent remote-addr)
             nil)))))))

(defun reset-session-secret ()
  "Sets *SESSION-SECRET* to a new random value. All old sessions will
cease to be valid."
  (setq *session-secret* (create-random-string 10 36)))

(defun reset-sessions (&optional (acceptor *acceptor*))
  "Removes ALL stored sessions of ACCEPTOR."
  (with-session-lock-held ((session-db-lock acceptor))
    (loop for (nil . session) in (session-db acceptor)
          do (acceptor-remove-session acceptor session))
    (setq *session-db* nil))
  (values))
