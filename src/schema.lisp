(in-package :cl-sqs)

(defconstantsafe +visibility-default+ 60)
(defconstantsafe +visibility-max+ 86400)
(defconstantsafe +visibility-min+ 0)

(defconstantsafe +retention-min+ 1) 
(defconstantsafe +retention-max+ 336) 
(defconstantsafe +retention-default+ 24) 


(defun validate-fields (alist definitions)
  (let ((failed)) 
    (values (mapcar 
              (lambda (def)
                (let* ((field (assoc (car def) alist))
                       (parser (or (getf (cadr def) :parser) #'identity))
                       (value (if (car field)
                                  (and (cadr field)
                                       (ignore-errors
                                         (funcall parser (cadr field))))
                                  (getf (cadr def) :default))))
                  (if (and (print def) (print parser) value 
                           (funcall (getf (cadr def) :validator) value))
                      (list (car field) value)
                      (push (car field) failed))))
              definitions)
            failed)))

(defmacro defschema (name definitions)
  `(defun ,name (alist)
     (validate-fields alist ',(mapc
                                (lambda (def)
                                  (setf (getf (cadr def) :validator)
                                        `(lambda (value)
                                           ,(getf (cadr def) :validator))))
                                (copy-list definitions)))))

(defschema change-visibility-schema 
           (:visibility-timeout (>= +visibility-max+ value +visibility-min+)
            :parser #'parse-integer)
           (:id (= (length value) 10) :default "carlo"))

(defschema enqueue-schema 
           (:visibility-timeout (>= +visibility-max+ value +visibility-min+)
            :default +visibility-default+
            :parser #'parse-integer)
           (:deduplication-id (<= (length value) 128)
            :default :NULL)
           (:retention-timeout (>= +retention-max+ value +retention-min+)
            :parser #'parse-integer
            :default +retention-default+))

(defschema dequeue-schema 
           ((:visibility-timeout 
              (:validator (>= +visibility-max+ value +visibility-min+)
               :default +visibility-default+  
               :parser #'parse-integer))))

(defschema delete-message-schema 
           (:id (= (length value) 10)))

(dequeue-schema '((:visibility-timeout "-23313")))
(dequeue-schema '((:visibility-timeout "23313")))
(dequeue-schema '((:visbility-timeout "-23313")))

(list (lambda () 1))
