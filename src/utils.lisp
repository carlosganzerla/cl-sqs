(in-package #:cl-sqs)

(defun symbol-to-kw (symbol)
  (intern (symbol-name symbol) :keyword))

(defun str-to-kw (str)
  (intern (string-upcase str) :keyword))

(let ((read-files (make-hash-table)))
  (defun read-file-memo (filename)
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

(defun try-get-env (name default)
  (or (sb-ext:posix-getenv name) default))

(defun validate-uuid (uuid)
  (labels ((group (from len)
             (dotimes (i len)
               (let ((c (char uuid (+ i from))))
                 (when (not (or (>= 102 (char-code c) 97)
                                (>= 57 (char-code c) 48)))
                   (return (list nil (+ i from) c))))))
           (dash (pos)
             (let ((c (char uuid pos)))
               (when (char/= #\- c)
                 (list nil pos c))))
           (check-len ()
             (when (/= (length uuid) 36)
               (list nil nil nil))))
    (let ((result (or (check-len) (group 0 8) (dash 8) (group 9 4) (dash 13)
                      (group 14 4) (dash 18) (group 19 4) (dash 23)
                      (group 24 12))))
      (if result
        (values-list result)
        (values t nil nil)))))
