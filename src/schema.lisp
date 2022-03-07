(in-package :cl-sqs)

(defconstantsafe +visibility-default+ 60)
(defconstantsafe +visibility-max+ 86400)
(defconstantsafe +visibility-min+ 0)

(defconstantsafe +retention-min+ 1) 
(defconstantsafe +retention-max+ 336) 
(defconstantsafe +retention-default+ 24) 


(defmacro defschema (name &rest definitions)
  `(defun ,name (alist)
     (let ((sanitized 
             (mapcar 
               (lambda (field)
                 (case (car field)
                   ,@(mapcar 
                       (lambda (def)
                         `(,(car def) 
                            (let* ((parser (or ,(getf def :parser) #'identity))
                                   (value (funcall parser (cadr field))))
                              (if value
                                  (if ,(cadr def)
                                      (list (car field) value)
                                      (return-from ,name (values nil field)))
                                  (list (car field) (getf field :default))))))
                       definitions)))
               (remove-if-not (lambda (field)
                                (assoc (car field) ',definitions)) alist))))
       (values t sanitized))))

(defschema change-visibility-schema 
           (:visibility-timeout (>= +visibility-max+ value +visibility-min+)
            :parser #'parse-integer)
           (:id (= (length value) 10)))

(defun validate (param)

  )

((:visibility-timeout (if value
                          (>= +visibility-max+ value +visibility-min+)
                          +visibility-default+))
 (:id (and value (= 10 (length value)))))

(change-visibility-schema '((:visibility-timeout "500")
                            (:id "2123456780")))
