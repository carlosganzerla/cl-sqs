(in-package #:cl-sqs)

(defvar *request*)
(defvar *content-type*)
(defvar *db* (make-database))

(defconstantsafe +path+ "/queue")
(defconstantsafe +max-payload-size+ 65535)
(defconstantsafe +content-type+ "text/plain") 


(define-condition request-error (error)
  ((status-code :initarg :status-code :accessor status-code
                :type fixnum)))

(defun content (content)
  `(200 (:content-type ,+content-type+) ,(list content)))

(defun empty (code)
  `(,code (:content-type ,+content-type+) ("")))

(defun abort-with-code (code)
  (error 'request-error :status-code code))

(defmacro with-request (params &body body)
  `(handler-case
     (destructuring-bind (&key ,@params)(parse-qs (getf *request*
                                                        :query-string))
       (unless (string= +path+ (getf *request* :path-info))
         (abort-with-code 404))
       (unless (string= +content-type+ (getf *request* :content-type))
         (abort-with-code 406))
       ,@body)
     (sb-kernel::arg-count-error () (abort-with-code 422))
     (type-error () (abort-with-code 422))))

(defun get-handler ()
  (with-request ((visibility-timeout +visibility-default+))
    (dequeue *db* (parse-int visibility-timeout visibility-timeout-t))))

(defun patch-handler ()
  (with-request (visibility-timeout) 
    (change-visibility *db* (parse-int visibility-timeout
                                       visibility-timeout-t))))

(defun delete-handler ()
  (with-request (id) 
    (delete-message *db* id)))

(defun post-handler ()
  (with-request ((deduplication-id :NULL)
                 (visibility-timeout +visibility-default+)
                 (retention-timeout +retention-default+))
    (enqueue *db* (parse-int visibility-timeout visibility-timeout-t))))

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
          (content response)
          (empty 204)))
    (request-error (c) (empty (status-code c)))))

(defun start ()
  (woo:run #'request-handler))
