;; -*- Lisp -*-

(defpackage :make-docstrings
  (:use :cl))

(in-package :make-docstrings)

(defclass formatting-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((understream :initarg :understream
                :reader understream)
   (width :initarg :width
          :initform (error "missing :width argument to formatting-stream creation")
          :reader width)
   (column :initform 0
           :accessor column)
   (word-wrap-p :initform t
                :accessor word-wrap-p)
   (word-buffer :initform (make-array 1000
                                      :element-type 'character
                                      :adjustable t
                                      :fill-pointer 0)
                :reader word-buffer)))

(defun write-char% (char stream)
  (incf (column stream))
  (write-char char (understream stream)))

(defun print-newline (stream)
  (write-char #\Newline (understream stream))
  (setf (column stream) 0))

(defun buffer-not-empty-p (stream)
  (plusp (length (word-buffer stream))))

(defun maybe-flush-word (stream)
  (when (buffer-not-empty-p stream)
    (cond
      ((< (width stream) (+ (column stream) (length (word-buffer stream))))
       (print-newline stream))
      ((plusp (column stream))
       (write-char% #\Space stream)))
    (loop for char across (word-buffer stream)
          do (write-char% char stream))
    (setf (fill-pointer (word-buffer stream)) 0)))

(defmethod trivial-gray-streams:stream-write-char ((stream formatting-stream) char)
  (if (word-wrap-p stream)
      (cond
        ((eql #\Space char)
         (maybe-flush-word stream))
        ((eql #\Newline char)
         (maybe-flush-word stream)
         (print-newline stream))
        (t
         (vector-push-extend char (word-buffer stream))))
      (write-char char (understream stream))))

(defmethod trivial-gray-streams:stream-line-column (stream)
  (+ (column stream) (length (word-buffer stream))))

(defmethod trivial-gray-streams:stream-write-string ((stream formatting-stream) string &optional start end)
  (loop for i from (or start 0) below (or end (length string))
        do (write-char (char string i) stream)))

(defmethod trivial-gray-streams:stream-terpri ((stream formatting-stream))
  (write-char #\Newline stream))

(defmethod close ((stream formatting-stream) &key abort)
  (unless abort
    (maybe-flush-word stream)))

(defmethod (setf word-wrap-p) :before (new-value (stream formatting-stream))
  (maybe-flush-word stream)
  (when (buffer-not-empty-p stream)
    (print-newline stream)))

(defun test-wrap-stream (text)
  (with-output-to-string (s)
    (with-open-stream (s (make-instance 'formatting-stream :understream s :width 20))
      (write-string text s)
      (setf (word-wrap-p s) nil)
      (format s "~&OFF~%")
      (write-string text s)
      (format s "~&ON~%")
      (setf (word-wrap-p s) t)
      (write-string text s))))

(defmacro replace-regexp (place regex replacement)
  `(setf ,place (cl-ppcre:regex-replace-all ,regex ,place ,replacement)))

(defun collapse-whitespace (string)
  (replace-regexp string "[ \\t]*\\n[ \\t]*" #.(make-string 1 :initial-element #\Newline))
  (replace-regexp string "(?<!\\n)\\n" " ")
  (remove #\Newline string))

(defvar *output*)

(defun make-docstring% (node transform)
  (stp:do-children (child node)
    (typecase child
      (stp:text
       (write-string (funcall transform (stp:data child)) *output*))
      (stp:element
       (ecase (intern (string-upcase (stp:local-name child)) :keyword)
         (:p
          (terpri *output*)
          (terpri *output*)
          (make-docstring% child transform))
         ((:a :code :tt :blockquote :span :ul)
          (make-docstring% child transform))
         ((:li)
           (make-docstring% child transform)
           (terpri *output*))
         ((:ref :arg :em :i)
          (make-docstring% child (alexandria:compose #'string-upcase transform)))
         ((:sup)
          ;; skip
          )
         (:pre
          (terpri *output*)
          (terpri *output*)
          (setf (word-wrap-p *output*) nil)
          (make-docstring% child #'identity)
          (setf (word-wrap-p *output*) t)
          (terpri *output*)))))))

(defun make-docstring (description-node)
  (with-output-to-string (s)
    (with-open-stream (*output* (make-instance 'formatting-stream :understream s :width 75))
      (make-docstring% description-node #'collapse-whitespace))))

(defun maybe-qualify-name (name package-name)
  (if (find #\: name)
      name
      (format nil "~A:~A" package-name name)))

(defun get-doc-entry-type (node)
  (let ((basic-type (intern (string-upcase (stp:local-name node)) :keyword)))
    (if (eq basic-type :function)
        (if (stp:attribute-value node "generic") ; FIXME: "no" not recognized
            :generic-function
            :function)
        basic-type)))

(defun skip-to (stream char)
  (loop until (eql char (peek-char nil stream))
        do (read-char stream)))

(defun get-function-docstring (source-string position)
  (with-input-from-string (s source-string :start (1+ position))
    (read s)                            ; DEFUN
    (read s)                            ; function name
    (read s)                            ; argument list
    (skip-to s #\")
    (values (file-position s)
            (read s)
            (file-position s))))

(defun get-def*-docstring (source-string position)
  (with-input-from-string (s source-string :start (1+ position))
    (read s)                            ; DEFCLASS/DEFINE-CONDITION/DEFGENERIC
    (read s)                            ; name
    (read s)                            ; arguments/supers
    (loop
      (let* ((start-of-clause (file-position s))
             (clause (read s)))
        (format t "first of clause: ~S~%" (first clause))
        (when (eql (first clause) :documentation)
          (file-position s start-of-clause)
          (skip-to s #\()
          (read-char s)
          (read s)                      ; :DOCUMENTATION
          (skip-to s #\")
          (return (values (file-position s)
                          (read s)
                          (file-position s))))))))

(defun get-special-variable-docstring (source-string position)
  nil)

(defun get-doc-function (type)
  (case type
    (:function 'get-function-docstring)
    ((:generic-function :class) 'get-def*-docstring)
    (:special-variable 'get-special-variable-docstring)))

(defun source-location-flatten (location-info)
  (apply #'append (rest (find :location (rest location-info) :key #'first))))

(defvar *files*)

(defclass file ()
  ((file-pathname :initarg :file-pathname
                  :reader file-pathname)
   (original-contents :reader original-contents)
   (contents :initarg :contents
             :accessor contents)))

(defmethod initialize-instance :after ((file file) &key contents)
  (setf (slot-value file 'original-contents) contents))

(defun file-contents (pathname)
  (unless (gethash pathname *files*)
    (setf (gethash pathname *files*)
          (make-instance 'file
                         :file-pathname pathname
                         :contents (alexandria:read-file-into-string pathname))))
  (contents (gethash pathname *files*)))

(defun get-source-docstring (get-doc-function symbol-name)
  (let ((definitions (remove-if (lambda (definition)
                                  (or (cl-ppcre:scan "(?i)^\\s*\\(defmethod\\s" (first definition))
                                      (eql (first (second definition)) :error)))
                                (swank:find-definitions-for-emacs symbol-name))))
    (case (length definitions)
      (0 "NO DEFINITIONS FOUND")
      (1 (let ((source-location (source-location-flatten (first definitions))))
           (funcall get-doc-function (alexandria:read-file-into-string (getf source-location :file)) (getf source-location :position))))
      (2 "MULTIPLE DEFINITIONS FOUND"))))

(defun parse-doc (pathname default-package-name)
  (let ((*files* (make-hash-table :test #'equal)))
    (xpath:with-namespaces (("clix" "http://bknr.net/clixdoc"))
      (xpath:do-node-set (node (xpath:evaluate "//*[clix:description!='']" (cxml:parse pathname (stp:make-builder))))
        (let ((type (get-doc-entry-type node))
              (symbol-name (maybe-qualify-name (stp:attribute-value node "name") default-package-name)))
          (format t "~%~%--- found element: ~A ~A~%~S~%DOCSTRING: ~A~%"
                  type
                  symbol-name
                  (swank:find-definitions-for-emacs symbol-name)
                  (alexandria:when-let (get-doc-function (get-doc-function type))
                    (get-source-docstring get-doc-function symbol-name)))
          (xpath:do-node-set (description (xpath:evaluate "clix:description" node))
            (write-string (make-docstring description))))))))
