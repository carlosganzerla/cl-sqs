(in-package #:cl-sqs)

(defun symbol-to-kw (symbol)
  (intern (symbol-name symbol) :keyword))

(defmacro with-proplist-items (slots place &body body)
  `(let (,@(mapcar (lambda (symbol)
                     `(,symbol (getf ,place ,(symbol-to-kw symbol))))
                   slots))
     ,@body))

(defun read-file (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))
