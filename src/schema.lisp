(in-package :cl-sqs)

(defconstantsafe +visibility-default+ 60)

(deftype visibility-timeout ()
  `(integer 0 86400))

(deftype retention-timeout ()
  `(integer 1 336))

(defun length-up-to (x)
  (>= (length x) 128))

(deftype deduplication-id ()
  (and (string) (satisfies )))

(defconstantsafe +retention-min+ 1) 
(defconstantsafe +retention-max+ 336) 
(defconstantsafe +retention-default+ 24) 

(defmacro d-bind (params form &body body)
  `(destructuring-bind (,@params &allow-other-keys) ,form
     (declare (ignorable ,@(remove-if (lambda (p)
                                        (char= (aref (symbol-name p) 0) #\&)) 
                                      params)))
     ,@body))

(defmacro defschema (name &body definitions)
  (flet ((conc-symbol (&rest symbols)
           (intern (apply #'concatenate 'string
                          (mapcar #'symbol-name symbols)))))
    `(progn 
       (defstruct (,name (:constructor ,(conc-symbol '%make- name)))
         ,@(mapcar (lambda (def)
                     (d-bind (var &key default type) def
                       `(,var ,default :type ,type :read-only t)))
                   definitions))
       (defun ,(conc-symbol name '-schema)
         (&key ,@(mapcar (lambda (def)
                           (d-bind (_ &key default) def
                             `(,(car def) ,default))) 
                         definitions))
         (,(conc-symbol '%make- name)
           ,@(mapcan (lambda (def)
                       (d-bind (_ &key parser) def
                         `(,(symbol-to-kw (car def))
                            (funcall ,(or parser #'identity) ,(car def)))))
                     definitions))))))

(defschema dequeue-params
  (visibility-timeout :default +visibility-default+
                      :parser #'parse-integer
                      :type visibility-timeout))

(ignore-errors (dequeue-params-schema :visibility-timeout "43"))

(typep -1 'visibility-timeout)

(typep "carlo" '(string 128))
