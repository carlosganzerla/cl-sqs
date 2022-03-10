(in-package :cl-sqs)

(defconstantsafe +visibility-default+ 60)
(defconstantsafe +visibility-max+ 86400)
(defconstantsafe +visibility-min+ 0)

(defconstantsafe +retention-min+ 1) 
(defconstantsafe +retention-max+ 336) 
(defconstantsafe +retention-default+ 24) 


(defun %validation-case (name def field)
  `(,(car def) 
     (let* ((parser (or ,(getf def :parser) #'identity))
            (default ,(getf def :default)))
       (cond ((and (not (cadr ,field)) default)
              (list (car ,field) default))
             ((and (cadr ,field))
              (let ((value (ignore-errors (funcall parser (cadr ,field)))))
                (if (and value ,(cadr def))
                    (list (car ,field) value)
                    (return-from ,name (values nil ,field)))))
             (t (return-from ,name (values nil ,field)))))))

(defmacro defschema (name &rest definitions)
  `(defun ,name (alist)
    (let* ((defined (remove-if-not (lambda (field)
                                     (assoc (car field) ',definitions))
                                   alist))
           (sanitized (mapcar 
                        (lambda (field)
                          (case (car field)
                            ,@(mapcar 
                                (lambda (def)
                                  (%validation-case name def
                                                    'field))
                                definitions)))
                        defined)))
      (values t sanitized))))

(defschema change-visibility-schema 
           (:visibility-timeout (>= +visibility-max+ value +visibility-min+)
            :parser #'parse-integer)
           (:id (= (length value) 10) :default "1234567890"))

(defschema enqueue-schema 
           (:visibility-timeout (>= +visibility-max+ value +visibility-min+)
            :parser #'parse-integer)
           (:deduplication-id (= (length value) 10))
           (:deduplication-id (= (length value) 10))
           )



(change-visibility-schema '((:visibility-timeout "500")
                            (:id nil)))

(change-visibility-schema '((:visibility-timeout "bunda")
                            (:id nil)))

(case t
  (:id 1))
