;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/session.lisp,v 1.12 2008/02/13 16:02:18 edi Exp $

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

(defvar *session-data-lock* (make-recursive-lock "session-data-lock")
  "A lock to prevent two threads from modifying *SESSION-DATA* at the
same time.")

(let ((session-id-counter 0))
  (defun get-next-session-id ()
    "Returns the next sequential session id."
    (incf session-id-counter)))

(let ((global-session-usage-counter 0))
  (defun count-session-usage ()
    "Counts session usage globally and triggers session gc if necessary." 
    (when (and *session-gc-frequency*
               (zerop (mod (incf global-session-usage-counter)
                           *session-gc-frequency*)))
      (session-gc))))


(defclass session ()
  ((session-id :initform (get-next-session-id)
               :reader session-id
               :type integer
               :documentation "The unique ID \(an INTEGER) of the session.")
   (session-string :reader session-string
                   :documentation "The session strings encodes enough
data to safely retrieve this session. It is sent to the browser as a
cookie value or as a GET parameter.")
   (user-agent :initform (user-agent *request*)
               :reader session-user-agent
               :documentation "The incoming 'User-Agent' header that
was sent when this session was created.")
   (remote-addr :initform (real-remote-addr *request*)
                :reader session-remote-addr
                :documentation "The remote IP address of the client when
this sessions was started as returned by REAL-REMOTE-ADDR.")
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
   (session-counter :initform 0
                    :reader session-counter
                    :documentation "The number of times this session
has been used.")
   (max-time :initarg :max-time
             :initform *session-max-time*
             :accessor session-max-time
             :type fixnum
             :documentation "The time \(in seconds) after which this
session expires if it's not used."))
  (:documentation "SESSION objects are automatically maintained
by Hunchentoot. They should not be created explicitly with
MAKE-INSTANCE but implicitly with START-SESSION. Note that
SESSION objects can only be created when the special variable
*REQUEST* is bound to a REQUEST object."))

(defun encode-session-string (id user-agent remote-addr start)
  "Create a uniquely encoded session string based on the values ID,
USER-AGENT, REMOTE-ADDR, and START"
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
  "Removes sessions from *session-data* which are too old - see
SESSION-TOO-OLD-P."
  (with-recursive-lock-held (*session-data-lock*)
    (setq *session-data*
            (loop for id-session-pair in *session-data*
                  for (nil . session) = id-session-pair
                  when (session-too-old-p session)
                    do (funcall *session-removal-hook* session)
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
  (with-rebinding (symbol)
    (with-unique-names (place %session)
      `(with-recursive-lock-held (*session-data-lock*)
         (let* ((,%session (or ,session (start-session)))
                (,place (assoc ,symbol (session-data ,%session) :test #'eq)))
           (cond
             (,place
              (setf (cdr ,place) ,new-value))
             (t
              (push (cons ,symbol ,new-value)
                    (slot-value ,%session 'session-data))
              ,new-value)))))))

(defun delete-session-value (symbol &optional (session *session*))
  "Removes the value associated with SYMBOL from the current session
object if there is one."
  (when session
    (setf (slot-value session 'session-data)
            (delete symbol (session-data session)
                    :key #'car :test #'eq)))
  (values))

(defun session-cookie-value (&optional (session (session *request*)))
  "Returns a string which can be used to safely restore the
session if as session has already been established. This is used
as the value stored in the session cookie or in the corresponding
GET parameter."
  (and session
       (format nil
               "~A:~A"
               (session-id session)
               (session-string session))))

(defun start-session ()
  "Returns the current SESSION object. If there is no current session,
creates one and updates the corresponding data structures. In this
case the function will also send a session cookie to the browser."
  (count-session-usage)
  (let ((session (session *request*)))
    (when session
      (return-from start-session session))
    (setf session (make-instance 'session)
          (session *request*) session)
    (with-recursive-lock-held (*session-data-lock*)
      (setq *session-data* (acons (session-id session) session *session-data*)))
    (set-cookie *session-cookie-name*
                :value (session-cookie-value session)
                :path "/")
    (setq *session* session)))

(defun remove-session (session)
  "Completely removes the SESSION object SESSION from Hunchentoot's
internal session database."
  (with-recursive-lock-held (*session-data-lock*)
    (funcall *session-removal-hook* session)
    (setq *session-data*
            (delete (session-id session) *session-data*
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
          (cdr (assoc id *session-data* :test #'=))))
    (when (and session
               (session-too-old-p session))
      (when *reply*
        (log-message* :info "Session with ID ~A too old" id))
      (remove-session session)
      (setq session nil))
    session))

(defun session-verify (request)
  "Tries to get a session identifier from the cookies \(or
alternatively from the GET parameters) sent by the client. This
identifier is then checked for validity against the REQUEST object
REQUEST. On success the corresponding session object \(if not too old)
is returned \(and updated). Otherwise NIL is returned."
  (let ((session-identifier (or (cookie-in *session-cookie-name* request)
                                (get-parameter *session-cookie-name* request))))
    (unless (and session-identifier
                 (stringp session-identifier)
                 (plusp (length session-identifier)))
      (return-from session-verify nil))
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
           ;; The session key presented by the client is valid.
           (incf (slot-value session 'session-counter))
           (setf (slot-value session 'last-click) (get-universal-time))
           session)
          (session
           ;; The session ID pointed to an existing session, but the
           ;; session string did not match the expected session
           ;; string.  Report to the log file, remove the session to
           ;; make sure that it can't be used again.  The original
           ;; legitimate user will be required to log in again.
           (log-message* :warning
                         "Fake session identifier '~A' (User-Agent: '~A', IP: '~A')"
                         session-identifier user-agent remote-addr)
           (remove-session session)
           nil)
          (t
           ;; No session was found under the ID given, presumably
           ;; because it has expired.
           (log-message* :info
                         "No session for session identifier '~A' (User-Agent: '~A', IP: '~A')"
                         session-identifier user-agent remote-addr)
           nil))))))

(defun reset-sessions ()
  "Removes ALL stored sessions and creates a new session secret."
  (reset-session-secret)
  (with-recursive-lock-held (*session-data-lock*)
    (loop for (nil . session) in *session-data*
          do (funcall *session-removal-hook* session))
    (setq *session-data* nil))
  (values))

(defmacro do-sessions ((var &optional result-form) &body body)
  "Executes BODY with VAR bound to each existing SESSION object
consecutively. Returns the values returned by RESULT-FORM unless
RETURN is executed. The scope of the binding of VAR does not include
RESULT-FORM."
  (let ((=temp= (gensym)))
    `(dolist (,=temp= *session-data* ,result-form)
      (let ((,var (cdr ,=temp=)))
        ,@body))))