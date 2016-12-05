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

(in-package :hunchentoot-test)

(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*)))

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,@body))

(defun hunchentoot-link ()
  (with-html-output (*standard-output*)
    (:a :href "http://weitz.de/hunchentoot/" "Hunchentoot")))

(defun menu-link ()
  (with-html-output (*standard-output*)
    (:p (:hr
         (:a :href "/hunchentoot/test" "Back to menu")))))

(defmacro with-lisp-output ((var) &body body)
  `(let ((*package* (find-package :hunchentoot-test-user)))
     (with-output-to-string (,var #+:lispworks nil
                                  #+:lispworks :element-type
                                  #+:lispworks 'lw:simple-char)
       ,@body)))

(defmacro info-table (&rest forms)
  (let ((=value= (gensym))
        (=first= (gensym)))
    `(with-html-output (*standard-output*)
       (:p (:table :border 1 :cellpadding 2 :cellspacing 0
            (:tr (:td :colspan 2
                  "Some Information "
                  (hunchentoot-link)
                  " provides about this request:"))
            ,@(loop for form in forms
                    collect `(:tr (:td :valign "top"
                                   (:pre :style "padding: 0px"
                                    (esc (with-lisp-output (s) (pprint ',form s)))))
                              (:td :valign "top"
                               (:pre :style "padding: 0px"
                                (esc (with-lisp-output (s)
                                       (loop for ,=value= in (multiple-value-list ,form)
                                             for ,=first= = t then nil
                                             unless ,=first=
                                             do (princ ", " s)
                                             do (pprint ,=value= s))))))))))
       (menu-link))))

(defun authorization-page ()
  (multiple-value-bind (user password)
      (authorization)
    (cond ((and (equal user "nanook")
                (equal password "igloo"))
           (with-html
             (:html
              (:head (:title "Hunchentoot page with Basic Authentication"))
              (:body
               (:h2 (hunchentoot-link)
                " page with Basic Authentication")
               (info-table (header-in* :authorization)
                           (authorization))))))
          (t
           (require-authorization)))))

(defparameter *test-image*
  (load-time-value
   (with-open-file (in (make-pathname :name "fz" :type "jpg" :version nil
                                      :defaults *this-file*)
                       :element-type 'flex:octet)
     (let ((image-data (make-array (file-length in)
                                   :element-type 'flex:octet)))
       (read-sequence image-data in)
       image-data))))

(defun image-ram-page ()
  (setf (content-type*) "image/jpeg")
  *test-image*)

(let ((count 0))
  (defun info ()
    (with-html
      (:html
       (:head (:title "Hunchentoot Information"))
       (:body
        (:h2 (hunchentoot-link) " Information Page")
        (:p "This page has been called "
         (:b
          (fmt "~[~;once~;twice~:;~:*~R times~]" (incf count)))
         " since its handler was compiled.")
        (info-table (host)
                    (acceptor-address *acceptor*)
                    (acceptor-port *acceptor*)
                    (remote-addr*)
                    (remote-port*)
                    (real-remote-addr)
                    (request-method*)
                    (script-name*)
                    (query-string*)
                    (get-parameters*)
                    (headers-in*)
                    (cookies-in*)
                    (user-agent)
                    (referer)
                    (request-uri*)
                    (server-protocol*)))))))

(defun oops ()
  (with-html
    (log-message* :error "Oops \(error log level).")
    (log-message* :warning "Oops \(warning log level).")
    (log-message* :info "Oops \(info log level).")
    (error "Errors were triggered on purpose.  Check your error log.")
    (:html
     (:body "You should never see this sentence..."))))

(defun redir ()
  (redirect "/hunchentoot/test/info.html?redirected=1"))

(defun forbidden ()
  (setf (return-code*) +http-forbidden+)
  nil)

(defun cookie-test ()
  (set-cookie "pumpkin" :value "barking")
  (no-cache)
  (with-html
    (:html
     (:head (:title "Hunchentoot cookie test"))
     (:body
      (:h2 (hunchentoot-link)
       " cookie test")
      (:p "You might have to reload this page to see the cookie value.")
      (info-table (cookie-in "pumpkin")
                  (mapcar 'car (cookies-in*)))))))

(defun session-test ()
  (let ((new-foo-value (post-parameter "new-foo-value")))
    (when new-foo-value
      (setf (session-value 'foo) new-foo-value)))
  (let ((new-bar-value (post-parameter "new-bar-value")))
    (when new-bar-value
      (setf (session-value 'bar) new-bar-value)))
  (no-cache)
  (with-html
    (:html
     (:head (:title "Hunchentoot session test"))
     (:body
      (:h2 (hunchentoot-link)
       " session test")
      (:p "Use the forms below to set new values for "
       (:code "FOO")
       " or "
       (:code "BAR")
       ". You can later return to this page to check if
they're still set. Also, try to use another browser at the same
time or try with cookies disabled.")
      (:p (:form :method :post
           "New value for "
           (:code "FOO")
           ": "
           (:input :type :text
            :name "new-foo-value"
            :value (or (session-value 'foo) ""))))
      (:p (:form :method :post
           "New value for "
           (:code "BAR")
           ": "
           (:input :type :text
            :name "new-bar-value"
            :value (or (session-value 'bar) ""))))
      (info-table (session-cookie-name *acceptor*) 
                  (cookie-in (session-cookie-name *acceptor*))
                  (mapcar 'car (cookies-in*))
                  (session-value 'foo)
                  (session-value 'bar))))))

(defun parameter-test (&key (method :get) (charset :iso-8859-1))
  (no-cache)
  (recompute-request-parameters :external-format
                                (flex:make-external-format charset :eol-style :lf))
  (setf (content-type*)
        (format nil "text/html; charset=~A" charset))
  (with-html
    (:html
     (:head (:title (fmt "Hunchentoot ~A parameter test" method)))
     (:body
      (:h2 (hunchentoot-link)
       (fmt " ~A parameter test with charset ~A" method charset))
      (:p "Enter some non-ASCII characters in the input field below
and see what's happening.")
      (:p (:form
           :method method
           "Enter a value: "
           (:input :type :text
            :name "foo")))
      (case method
        (:get (info-table (query-string*)
                          (map 'list 'char-code (get-parameter "foo"))
                          (get-parameter "foo")))
        (:post (info-table (raw-post-data)
                           (map 'list 'char-code (post-parameter "foo"))
                           (post-parameter "foo"))))))))

(defun parameter-test-latin1-get ()
  (parameter-test :method :get :charset :iso-8859-1))

(defun parameter-test-latin1-post ()
  (parameter-test :method :post :charset :iso-8859-1))

(defun parameter-test-utf8-get ()
  (parameter-test :method :get :charset :utf-8))

(defun parameter-test-utf8-post ()
  (parameter-test :method :post :charset :utf-8))

;; this should not be the same directory as *TMP-DIRECTORY* and it
;; should be initially empty (or non-existent)
(defvar *tmp-test-directory*
    #+(or :win32 :mswindows) #p"c:\\hunchentoot-temp\\test\\"
    #-(or :win32 :mswindows) #p"/tmp/hunchentoot/test/")

(defvar *tmp-test-files* nil)

(let ((counter 0))
  (defun handle-file (post-parameter)
    (when (and post-parameter
               (listp post-parameter))
      (destructuring-bind (path file-name content-type)
          post-parameter
        (let ((new-path (make-pathname :name (format nil "hunchentoot-test-~A"
                                                     (incf counter))
                                       :type nil
                                       :defaults *tmp-test-directory*)))
          ;; strip directory info sent by Windows browsers
          (when (search "Windows" (user-agent) :test 'char-equal)
            (setq file-name (cl-ppcre:regex-replace ".*\\\\" file-name "")))
          (rename-file path (ensure-directories-exist new-path))
          (push (list new-path file-name content-type) *tmp-test-files*))))))

(defun clean-tmp-dir ()
  (loop for (path . nil) in *tmp-test-files*
        when (probe-file path)
        do (ignore-errors (delete-file path)))
  (setq *tmp-test-files* nil))

(defun upload-test ()
  (let (post-parameter-p)
    (when (post-parameter "file1")
      (handle-file (post-parameter "file1"))
      (setq post-parameter-p t))
    (when (post-parameter "file2")
      (handle-file (post-parameter "file2"))
      (setq post-parameter-p t))
    (when (post-parameter "clean")
      (clean-tmp-dir)
      (setq post-parameter-p t)))
  (no-cache)
  (with-html
    (:html
     (:head (:title "Hunchentoot file upload test"))
     (:body
      (:h2 (hunchentoot-link)
       " file upload test")
      (:form :method :post :enctype "multipart/form-data"
       (:p "First file: "
        (:input :type :file
         :name "file1"))
       (:p "Second file: "
        (:input :type :file
         :name "file2"))
       (:p (:input :type :submit)))
      (when *tmp-test-files*
        (htm
         (:p
          (:table :border 1 :cellpadding 2 :cellspacing 0
           (:tr (:td :colspan 3 (:b "Uploaded files")))
           (loop for (path file-name nil) in *tmp-test-files*
                 for counter from 1
                 do (htm
                     (:tr (:td :align "right" (str counter))
                      (:td (:a :href (format nil "files/~A?path=~A"
                                             (url-encode file-name)
                                             (url-encode (namestring path)))
                            (esc file-name)))
                      (:td :align "right"
                       (str (ignore-errors
                              (with-open-file (in path)
                                (file-length in))))
                       "&nbsp;Bytes"))))))
         (:form :method :post
          (:p (:input :type :submit :name "clean" :value "Delete uploaded files")))))
      (menu-link)))))

(defun send-file ()
  (let* ((path (get-parameter "path"))
         (file-info (and path
                         (find path *tmp-test-files*
                               :key 'first :test (lambda (a b) (equal a (namestring b)))))))
    (unless file-info
      (setf (return-code*) +http-not-found+)
      (return-from send-file))
    (handle-static-file (first file-info) (third file-info))))

(defparameter *headline*
  (load-time-value              
   (format nil "Hunchentoot test menu (see file <code>~A</code>)"
           (truename (merge-pathnames (make-pathname :type "lisp") *this-file*)))))

(defvar *utf-8* (flex:make-external-format :utf-8 :eol-style :lf))

(defvar *utf-8-file* (merge-pathnames "UTF-8-demo.html" *this-file*)
  "Demo file stolen from <http://www.w3.org/2001/06/utf-8-test/>.")

(defun stream-direct ()
  (setf (content-type*) "text/html; charset=utf-8")
  (let ((stream (send-headers))
        (buffer (make-array 1024 :element-type 'flex:octet)))
    (with-open-file (in *utf-8-file* :element-type 'flex:octet)
      (loop for pos = (read-sequence buffer in)
            until (zerop pos) 
            do (write-sequence buffer stream :end pos)))))

(defun stream-direct-utf-8 ()
  (setf (content-type*) "text/html; charset=utf-8")
  (let ((stream (flex:make-flexi-stream (send-headers) :external-format *utf-8*)))
    (with-open-file (in (merge-pathnames "UTF-8-demo.html" *this-file*)
                        :element-type 'flex:octet)
      (setq in (flex:make-flexi-stream in :external-format *utf-8*))
      (loop for line = (read-line in nil nil)
            while line
            do (write-line line stream)))))

(defun stream-direct-utf-8-string ()
  (setf (content-type*) "text/html; charset=utf-8"
        (reply-external-format*) *utf-8*)
  (with-open-file (in (merge-pathnames "UTF-8-demo.html" *this-file*)
                      :element-type 'flex:octet)
    (let ((string (make-array (file-length in)
                              :element-type #-:lispworks 'character #+:lispworks 'lw:simple-char
                              :fill-pointer t)))
      (setf in (flex:make-flexi-stream in :external-format *utf-8*)
            (fill-pointer string) (read-sequence string in))
      string)))

(define-easy-handler (easy-demo :uri "/hunchentoot/test/easy-demo.html"
                                :default-request-type :post)
    (first-name last-name
                (age :parameter-type 'integer)
                (implementation :parameter-type 'keyword)
                (meal :parameter-type '(hash-table boolean))
                (team :parameter-type 'list))
  (with-html
    (:html
     (:head (:title "Hunchentoot \"easy\" handler example"))
     (:body
      (:h2 (hunchentoot-link)
       " \"Easy\" handler example")
      (:p (:form :method :post
           (:table :border 1 :cellpadding 2 :cellspacing 0
            (:tr
             (:td "First Name:")
             (:td (:input :type :text
                   :name "first-name"
                   :value (or first-name "Donald"))))
            (:tr
             (:td "Last name:")
             (:td (:input :type :text
                   :name "last-name"
                   :value (or last-name "Duck"))))
            (:tr
             (:td "Age:")
             (:td (:input :type :text
                   :name "age"
                   :value (or age 42))))
            (:tr
             (:td "Implementation:")
             (:td (:select :name "implementation"
                   (loop for (value option) in '((:lispworks "LispWorks")
                                                 (:allegro "AllegroCL")
                                                 (:cmu "CMUCL")
                                                 (:sbcl "SBCL")
                                                 (:openmcl "OpenMCL"))
                         do (htm
                             (:option :value value
                              :selected (eq value implementation)
                              (str option)))))))
            (:tr
             (:td :valign :top "Meal:")
             (:td (loop for choice in '("Burnt weeny sandwich"
                                        "Canard du jour"
                                        "Easy meat"
                                        "Muffin"
                                        "Twenty small cigars"
                                        "Yellow snow")
                        do (htm
                            (:input :type "checkbox"
                             :name (format nil "meal{~A}" choice)
                             :checked (gethash choice meal)
                             (esc choice))
                            (:br)))))
            (:tr
             (:td :valign :top "Team:")
             (:td (loop for player in '("Beckenbauer"
                                        "Cruyff"
                                        "Maradona"
                                        ;; without accent (for SBCL)
                                        "Pele"
                                        "Zidane")
                        do (htm
                            (:input :type "checkbox"
                             :name "team"
                             :value player
                             :checked (member player team :test 'string=)
                             (esc player))
                            (:br)))))
            (:tr
             (:td :colspan 2
              (:input :type "submit"))))))
      (info-table first-name
                  last-name
                  age
                  implementation
                  (loop :for choice :being :the :hash-keys :of meal :collect choice)
                  (gethash "Yellow snow" meal)
                  team)))))
                

(defun menu ()
  (with-html
    (:html
     (:head
      (:link :rel "shortcut icon"
       :href "/hunchentoot/test/favicon.ico" :type "image/x-icon")
      (:title "Hunchentoot test menu"))
     (:body
      (:h2 (str *headline*))
      (:table :border 0 :cellspacing 4 :cellpadding 4
       (:tr (:td (:a :href "/hunchentoot/test/info.html?foo=bar"
                  "Info provided by Hunchentoot")))
       (:tr (:td (:a :href "/hunchentoot/test/cookie.html"
                  "Cookie test")))
       (:tr (:td (:a :href "/hunchentoot/test/session.html"
                  "Session test")))
       (:tr (:td (:a :href "/hunchentoot/test/parameter_latin1_get.html"
                  "GET parameter handling with LATIN-1 charset")))
       (:tr (:td (:a :href "/hunchentoot/test/parameter_latin1_post.html"
                  "POST parameter handling with LATIN-1 charset")))
       (:tr (:td (:a :href "/hunchentoot/test/parameter_utf8_get.html"
                  "GET parameter handling with UTF-8 charset")))
       (:tr (:td (:a :href "/hunchentoot/test/parameter_utf8_post.html"
                  "POST parameter handling with UTF-8 charset")))
       (:tr (:td (:a :href "/hunchentoot/test/redir.html"
                  "Redirect \(302) to info page above")))
       (:tr (:td (:a :href "/hunchentoot/test/authorization.html"
                  "Authorization")
             " (user 'nanook', password 'igloo')"))
       (:tr (:td (:a :href "/hunchentoot/code/test-handlers.lisp"
                  "The source code of this test")))
       (:tr (:td (:a :href "/hunchentoot/test/image.jpg"
                  "Binary data, delivered from file")
             " \(a picture)"))
       (:tr (:td (:a :href "/hunchentoot/test/image-ram.jpg"
                  "Binary data, delivered from RAM")
             " \(same picture)"))
       (:tr (:td (:a :href "/hunchentoot/test/easy-demo.html"
                  "\"Easy\" handler example")))
       (:tr (:td (:a :href "/hunchentoot/test/utf8-binary.txt"
                  "UTF-8 demo")
             " \(writing octets directly to the stream)"))
       (:tr (:td (:a :href "/hunchentoot/test/utf8-character.txt"
                  "UTF-8 demo")
             " \(writing UTF-8 characters directly to the stream)"))
       (:tr (:td (:a :href "/hunchentoot/test/utf8-string.txt"
                  "UTF-8 demo")
             " \(returning a string)"))
       (:tr (:td (:a :href "/hunchentoot/test/upload.html"
                  "File uploads")))
       (:tr (:td (:a :href "/hunchentoot/test/forbidden.html"
                  "Forbidden \(403) page")))
       (:tr (:td (:a :href "/hunchentoot/test/oops.html"
                  "Error handling")
             " \(output depends on "
             (:a :href "http://weitz.de/hunchentoot/#*show-lisp-errors-p*"
              (:code "*SHOW-LISP-ERRORS-P*"))
             (fmt " \(currently ~S))" *show-lisp-errors-p*)))
       (:tr (:td (:a :href "/hunchentoot/foo"
                  "URI handled by")
             " "
             (:a :href "http://weitz.de/hunchentoot/#*default-handler*"
              (:code "*DEFAULT-HANDLER*")))))))))

(setq *dispatch-table*
      (nconc
       (list 'dispatch-easy-handlers
             (create-static-file-dispatcher-and-handler
              "/hunchentoot/test/image.jpg"
              (make-pathname :name "fz" :type "jpg" :version nil
                             :defaults *this-file*)
              "image/jpeg")
             (create-static-file-dispatcher-and-handler
              "/hunchentoot/test/favicon.ico"
              (make-pathname :name "favicon" :type "ico" :version nil
                             :defaults *this-file*))
             (create-folder-dispatcher-and-handler
              "/hunchentoot/code/"
              (make-pathname :name nil :type nil :version nil
                             :defaults *this-file*)
              "text/plain"))
       (mapcar (lambda (args)
                 (apply 'create-prefix-dispatcher args))
               '(("/hunchentoot/test/form-test.html" form-test)
                 ("/hunchentoot/test/forbidden.html" forbidden)
                 ("/hunchentoot/test/info.html" info)
                 ("/hunchentoot/test/authorization.html" authorization-page)
                 ("/hunchentoot/test/image-ram.jpg" image-ram-page)
                 ("/hunchentoot/test/cookie.html" cookie-test)
                 ("/hunchentoot/test/session.html" session-test)
                 ("/hunchentoot/test/parameter_latin1_get.html" parameter-test-latin1-get)
                 ("/hunchentoot/test/parameter_latin1_post.html" parameter-test-latin1-post)
                 ("/hunchentoot/test/parameter_utf8_get.html" parameter-test-utf8-get)
                 ("/hunchentoot/test/parameter_utf8_post.html" parameter-test-utf8-post)
                 ("/hunchentoot/test/upload.html" upload-test)
                 ("/hunchentoot/test/redir.html" redir)
                 ("/hunchentoot/test/oops.html" oops)
                 ("/hunchentoot/test/utf8-binary.txt" stream-direct)
                 ("/hunchentoot/test/utf8-character.txt" stream-direct-utf-8)
                 ("/hunchentoot/test/utf8-string.txt" stream-direct-utf-8-string)
                 ("/hunchentoot/test/files/" send-file)
                 ("/hunchentoot/test" menu)))))
