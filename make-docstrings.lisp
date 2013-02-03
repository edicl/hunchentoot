;; -*- Lisp -*-

(defpackage :make-docstrings
  (:use :cl)
  (:export #:update-doc))

(in-package :make-docstrings)

(defparameter *docstring-width* 70)

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
  (cond
    ((word-wrap-p stream)
     (cond
       ((eql #\Space char)
        (maybe-flush-word stream))
       ((eql #\Newline char)
        (maybe-flush-word stream)
        (print-newline stream))
       (t
        (when (member char '(#\( #\" #\\))
          (vector-push-extend #\\ (word-buffer stream)))
        (vector-push-extend char (word-buffer stream)))))
    (t
     (when (member char '(#\" #\\))
       (write-char #\\ (understream stream)))
     (write-char char (understream stream)))))

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

(defun xml-to-docstring% (node transform)
  (stp:do-children (child node)
    (typecase child
      (stp:text
       (write-string (funcall transform (stp:data child)) *output*))
      (stp:element
       (ecase (intern (string-upcase (stp:local-name child)) :keyword)
         (:p
          (terpri *output*)
          (terpri *output*)
          (xml-to-docstring% child transform))
         ((:a :code :tt :blockquote :span :ul)
          (xml-to-docstring% child transform))
         ((:li)
           (xml-to-docstring% child transform)
           (terpri *output*))
         ((:ref :arg :em :i)
          (xml-to-docstring% child (alexandria:compose #'string-upcase transform)))
         ((:sup)
          ;; skip
          )
         (:pre
          (terpri *output*)
          (terpri *output*)
          (setf (word-wrap-p *output*) nil)
          (xml-to-docstring% child #'identity)
          (setf (word-wrap-p *output*) t)
          (terpri *output*)))))))

(defun xml-to-docstring (description-node)
  (with-output-to-string (s)
    (with-open-stream (*output* (make-instance 'formatting-stream :understream s :width *docstring-width*))
      (xml-to-docstring% description-node #'collapse-whitespace))))

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

(defclass docstring ()
  ((start :initarg :start
          :reader start)
   (end :initarg :end
        :reader end)
   (source-text :initarg :source-text
                :reader source-text)
   (doc-text :initform nil
             :accessor doc-text)))

(defun read-docstring (s)
  (skip-to s #\")
  (let* ((start (1+ (file-position s)))
         (text (read s)))
    (loop for position = (file-position s)
          until (eql (peek-char nil s) #\")
          do (file-position s (1- position)))
    (read-char s)                       ; position at char after closing quote
    (make-instance 'docstring
                   :start start
                   :source-text text
                   :end (1- (file-position s)))))

(defun ensure-read-symbol (stream &rest symbols)
  (let ((got (read stream)))
    (unless (member got symbols :test #'string=)
      (error "expected one of ~S, got ~S" symbols got))
    got))

(defun get-simple-def-docstring (source-string position)
  (with-input-from-string (s source-string :start position)
    (let ((definer (ensure-read-symbol s 'defun 'defmacro 'defvar 'defparameter 'defvar-unbound)))
      (read s)                          ; name
      (unless (string= definer 'defvar-unbound)
        (read s))                       ; argument list/initial value
      (read-docstring s))))

(defun get-complex-def-docstring (source-string position)
  (with-input-from-string (s source-string :start position)
    (ensure-read-symbol s 'defclass 'define-condition 'defgeneric)
    (read s)                            ; name
    (read s)                            ; arguments/supers
    (loop
      (let* ((start-of-clause (file-position s))
             (clause (read s)))
        (when (eql (first clause) :documentation)
          (file-position s start-of-clause)
          (skip-to s #\()
          (read-char s)
          (read s)                      ; :DOCUMENTATION
          (return (read-docstring s)))))))

(defun get-doc-function (type)
  (case type
    ((:function :special-variable) 'get-simple-def-docstring)
    ((:generic-function :class) 'get-complex-def-docstring)))

(defun source-location-flatten (location-info)
  (apply #'append (rest (find :location (rest location-info) :key #'first))))

(defvar *files*)

(defclass file ()
  ((file-pathname :initarg :file-pathname
                  :reader file-pathname)
   (docstrings :initform nil
               :accessor docstrings)
   (contents :accessor contents)))

(defmethod initialize-instance :after ((file file) &key file-pathname)
  (setf (slot-value file 'contents) (alexandria:read-file-into-string file-pathname)))

(defun get-file (pathname)
  (or (gethash pathname *files*)
      (setf (gethash pathname *files*)
            (make-instance 'file
                           :file-pathname pathname))))

(defun record-docstring (doc-docstring get-doc-function symbol-name)
  (let ((definitions (remove-if (lambda (definition)
                                  (or (cl-ppcre:scan "(?i)^\\s*\\(defmethod\\s" (first definition))
                                      (eql (first (second definition)) :error)))
                                (swank:find-definitions-for-emacs symbol-name))))
    (case (length definitions)
      (0 (warn "no source location for ~A" symbol-name))
      (1 (let* ((source-location (source-location-flatten (first definitions)))
                (file (get-file (getf source-location :file))))
           (push (let ((docstring (funcall get-doc-function (contents file) (getf source-location :position))))
                   (setf (doc-text docstring) doc-docstring)
                   docstring)
                 (docstrings file))))
      (2 (warn "multiple source locations for ~A" symbol-name)))))

(defun parse-doc (pathname default-package-name)
  (let ((*files* (make-hash-table :test #'equal)))
    (xpath:with-namespaces (("clix" "http://bknr.net/clixdoc"))
      (xpath:do-node-set (node (xpath:evaluate "//*[clix:description!='']" (cxml:parse (pathname pathname) (stp:make-builder))))
        (let ((type (get-doc-entry-type node))
              (symbol-name (maybe-qualify-name (stp:attribute-value node "name") default-package-name)))
          (xpath:do-node-set (description (xpath:evaluate "clix:description" node))
            (alexandria:when-let (get-doc-function (get-doc-function type))
              (record-docstring (xml-to-docstring description)
                                get-doc-function symbol-name))))))
    *files*))

(defun edit-files (files)
  (dolist (file (alexandria:hash-table-values files))
    (when (docstrings file)
      (let ((previous-end 0)
            (s (make-string-output-stream)))
        (mapl (lambda (rest)
                (destructuring-bind (docstring &rest more) rest
                  (write-string (subseq (contents file) previous-end (start docstring)) s)
                  (write-string (doc-text docstring) s)
                  (if more
                      (setf previous-end (end docstring))
                      (write-string (subseq (contents file) (end docstring)) s))))
              (sort (copy-list (docstrings file))
                    #'<
                    :key #'start))
        (let ((new-contents (get-output-stream-string s)))
          (unless (equal new-contents (contents file))
            (with-open-file (f (file-pathname file) :direction :output :if-exists :rename)
              (write-string new-contents f))))))))

(defun update-doc (doc-pathname default-package-name)
  (edit-files (parse-doc doc-pathname default-package-name)))
