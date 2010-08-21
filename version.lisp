(in-package :cl-user)

(defpackage :hunchentoot-version
  (:use :cl)
  (:export #:*hunchentoot-version*))

(in-package :hunchentoot-version)

(defvar *hunchentoot-version* "1.1.0"
  "Hunchentoot's version number as a string.")
