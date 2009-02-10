;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/test/test.lisp,v 1.24 2008/03/06 07:46:53 edi Exp $

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

(in-package :hunchentoot-test)

(defun file-length-string (pathname)
  (with-open-file (f pathname)
    (princ-to-string (file-length f))))

(defun test-hunchentoot (base-url &key (make-cookie-jar (lambda () (make-instance 'drakma:cookie-jar))))
  (with-script-context (:base-url (format nil "~A/hunchentoot/test/" base-url))

    (format t "Request home page~%")
    (http-request "")
    (http-assert 'status-code 200)
    (http-assert-header :content-type "^text/html")

    (format t "Test cookies~%")
    (let ((cookie-jar (funcall make-cookie-jar)))
      (http-request "cookie.html" :cookie-jar cookie-jar)
      (http-request "cookie.html" :cookie-jar cookie-jar)
      (http-assert-body "(?ms)COOKIE-IN &quot;pumpkin&quot;.*&quot;barking&quot;"))

    (format t "Test session variables~%")
    (let ((cookie-jar (funcall make-cookie-jar)))
      (http-request "session.html" :cookie-jar cookie-jar
                    :method :post :parameters '(("new-foo-value" . "ABC") ("new-bar-value" . "DEF")))
      (http-request "session.html" :cookie-jar cookie-jar)
      ;; These assertions assume that SESSION-VALUE returns the found alist value as second value
      (http-assert-body "\(HUNCHENTOOT-TEST::FOO . &quot;ABC&quot;\)")
      (http-assert-body "\(HUNCHENTOOT-TEST::BAR . &quot;DEF&quot;\)"))

    (format t "Test GET parameters with foreign characters (Latin-1)~%")
    (http-request "parameter_latin1_get.html?foo=H%FChner")
    (http-assert-header :content-type "text/html; charset=ISO-8859-1")
    (http-assert-body "(72 252 104 110 101 114)")
    (http-assert-body "&quot;H&#xFC;hner&quot;")

    (format t "Test POST parameters with foreign characters (Latin-1)~%")
    (http-request "parameter_latin1_post.html"
                  :method :post :parameters (list (cons "foo" (format nil "H~Chner" #\Latin_Small_Letter_U_With_Diaeresis))))
    (http-assert-header :content-type "text/html; charset=ISO-8859-1")
    (http-assert-body "(72 252 104 110 101 114)")
    (http-assert-body "&quot;H&#xFC;hner&quot;")

    (format t "Test GET parameters with foreign characters (UTF-8)~%")
    (http-request "parameter_utf8_get.html?foo=H%C3%BChner")
    (http-assert-header :content-type "text/html; charset=UTF-8")
    (http-assert-body "(72 252 104 110 101 114)")
    (http-assert-body "&quot;H&#xFC;hner&quot;")

    (format t "Test POST parameters with foreign characters (UTF-8)~%")
    (http-request "parameter_utf8_post.html"
                  :method :post
                  :external-format-out :utf-8
                  :parameters (list (cons "foo" (format nil "H~Chner" #\Latin_Small_Letter_U_With_Diaeresis))))
    (http-assert-header :content-type "text/html; charset=UTF-8")
    (http-assert-body "(72 252 104 110 101 114)")
    (http-assert-body "&quot;H&#xFC;hner&quot;")

    (format t "Test redirection~%")
    (http-request "redir.html")
    (http-assert 'uri (lambda (uri)
                        (matches (princ-to-string uri) "info.html\\?redirected=1")))

    (format t "Test authorization~%")
    (http-request "authorization.html")
    (http-assert 'status-code 401)
    (http-request "authorization.html"
                  :basic-authorization '("nanook" "igloo"))
    (http-assert 'status-code 200)

    (format t "Request the zappa image~%")
    (http-request "image.jpg")
    (http-assert-header :content-length (file-length-string #P"fz.jpg"))
    (http-assert-header :content-type "image/jpeg")
    (http-assert 'body (complement #'mismatch) (file-contents #P"fz.jpg"))

    (format t "Request the zappa image from RAM~%")
    (http-request "image-ram.jpg")
    (http-assert-header :content-length (file-length-string #P"fz.jpg"))
    (http-assert-header :content-type "image/jpeg")
    (http-assert 'body (complement #'mismatch) (file-contents #P"fz.jpg"))

    (format t "Upload a file~%")
    (http-request "upload.html"
                  :method :post :parameters '(("clean" . "doit")))
    (http-request "upload.html"
                  :method :post :parameters '(("file1" #P"fz.jpg")))
    (http-request "upload.html")
    (http-assert-body (format nil "fz.jpg.*>~A&nbsp;Bytes" (file-length-string #P"fz.jpg")))))

