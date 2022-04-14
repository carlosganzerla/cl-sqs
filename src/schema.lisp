(in-package #:cl-sqs)

(defmacro defschema (name &body fields)
  (let* ((macro-name (intern (concatenate 'string (string name) "-BIND")))
         (field-params (mapcar #'car fields)))
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
       (defmacro ,macro-name (proplist vars &body body)
         `(destructuring-bind (&key ,@vars) (,',name ,proplist)
            ,@body)))))

(defun varchar128-p (str)
  (and (typep str 'string) (>= 128 (length str))))

(deftype varchar128 ()
  `(satisfies varchar128-p))

(deftype uuid ()
  `(satisfies validate-uuid))

(defschema dequeue-schema
  ((visibility-timeout 60) :target (integer 0 86400)
                           :converter #'parse-integer))

;; TODO: Validate UUID
(defschema delete-schema
  (message-receipt-id :target uuid))

(defschema enqueue-schema
  (message-group-id :target varchar128)
  ((deduplication-id :NULL) :target (or (eql :NULL) varchar128)))
