(in-package #:cl-sqs)

(defun symbol-to-kw (symbol)
  (intern (symbol-name symbol) :keyword))

(defun str-to-kw (str)
  (intern (string-upcase str) :keyword))

(let ((read-files (make-hash-table)))
  (defun read-file-lazy (filename)
    (if (gethash filename read-files)
        (gethash filename read-files)
        (with-open-file (stream filename)
          (let ((contents (make-string (file-length stream))))
            (read-sequence contents stream)
            (setf (gethash filename read-files) contents)
            contents)))))

(defmacro defconstantsafe (sym val)
  `(unless (boundp ',sym)
     (defconstant ,sym ,val)))

(defun tc (val type)
  (if (typep val type)
      val
      (error 'simple-type-error
             :datum val
             :expected-type type
             :format-control
             "Type ~A not satisfied. Datum: ~A"
             :format-arguments (list type val))))

(defun car-if-list (x)
  (if (listp x)
      (car x)
      x))

