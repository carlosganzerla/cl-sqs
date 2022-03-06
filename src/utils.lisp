(in-package #:cl-sqs)

(defun symbol-to-kw (symbol)
  (intern (symbol-name symbol) :keyword))

(defmacro with-proplist-items (slots place &body body)
  `(let (,@(mapcar (lambda (symbol)
                     `(,symbol (getf ,place ,(symbol-to-kw symbol))))
                   slots))
     ,@body))

