(in-package #:cl-sqs)

(defvar *request*)

(defvar *db*)

(defconstant +path+ "/queue")
(defconstant +json-content+ "application/json")
(defconstant +max-payload-size+ 65535)

(define-condition request-condition (error)
  ((status-code :initarg :status-code :accessor :status-code
                :type fixnum)))

(defun make-success (content)
  `(200 (:content-type "application/json") ,(list content)))

(defun make-empty ()
  '(204 (:content-type "application/json") ("")))

(defun make-error (code)
  `(,code (:content-type "text/plain") ("")))

(defun abort-with-code (code)
  (error 'request-condition :status-code code))

(defun read-payload ()
  (let* ((len (min +max-payload-size+ (getf *request* :content-length)))
         (octets (make-array len)))
    (when len
      (read-sequence octets (getf *request* :raw-body)))
    (flexi-streams:octets-to-string octets)))

(defun validate-params (mandatory &optional non-mandatory))

(defun validate-content-type (content-type)
  (unless (equal content-type (getf *request* :content-type))
    (abort-with-code 406)) 
  t)

(defun validate-path (path)
  (unless (equal path (getf *request* :path))
    (abort-with-code 404))
  t)

(defun get-handler ()
  (let ((url-params (and (validate-content-type nil) 
                         (validate-path +path+)
                         (extract-params nil '(:visibility-timeout))))
        (*db* (getf url-params :visibility-timeout)))))

(defun patch-handler ()
  (and (validate-content-type nil)
       (validate-path +path+)
       (extract-params '(:id :visibility-timeout))))

(defun delete-handler ()
  (and (validate-content-type nil) 
       (validate-path +path+)
       (extract-params '(:id))))

(defun post-handler ()
  (and (validate-content-type +json-content+)
       (validate-path +path+)
       (extract-params nil '(:deduplication-id :visibility-timeout))))

(defun method-handler ()
  (case (getf *request* :request-method)
    (:GET (get-handler))
    (:DELETE (delete-handler))
    (:PATCH (patch-handler))
    (:POST (post-handler))
    (t (abort-with-code 405))))


(defun request-handler (*request*)
  (handler-case 
    (let ((response (method-handler)))
      (if response
          (make-success response)
          (make-empty)))
    (request-error (c) (make-error (status-code c)))
    (condition () (make-error 500))))


(defun start ()
  (woo:run #'request-handler))
