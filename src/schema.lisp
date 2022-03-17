(in-package #:cl-sqs)

(defmacro defschema (name &body fields)
  (let* ((macro-name (intern (concatenate 'string (string name) "-BIND")))
         (field-params (mapcar #'car fields))
         (field-vars (mapcar #'car-if-list field-params)))
    `(progn 
       (defun ,name (proplist)
         (destructuring-bind (&key ,@field-params) proplist
           (append 
             ,@(mapcar 
                 (lambda (field)
                   (destructuring-bind (name &key
                                             (target t)
                                             (converter '#'identity)
                                             (from 'string)) field
                     (let ((name (car-if-list name)))
                       `(if (typep ,name ',target)
                            (list ,(symbol-to-kw name) ,name)
                            (list ,(symbol-to-kw name)
                                  (tc (funcall ,converter (tc ,name ',from))
                                      ',target))))))
                 fields))))
       (defmacro ,macro-name (proplist &body body)
         `(destructuring-bind (&key ,@',field-vars) 
            (,',name ,proplist)
            ,@body)))))

(defschema dequeue-schema
  ((visibility-timeout 60) :target (integer 0 86400)
                           :converter #'parse-integer))

(defschema enqueue-schema
  ((visibility-timeout 60) :target (integer 0 86400)
                           :converter #'parse-integer)
  ((retention-timeout 24) :target (integer 1 336)
                          :converter #'parse-integer)
  ((deduplication-id :NULL) :target (or (eql :NULL) string)))
