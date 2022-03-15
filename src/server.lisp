(in-package #:cl-sqs)

(defvar *request*)
(defvar *db*)

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
     (let ((,params (parse-qs (getf *request* :query-string)))) 
       (unless (equal +path+ (getf *request* :path-info))
         (abort-with-code 404))
       ,@body)
     (sb-kernel::arg-count-error () (abort-with-code 422))
     (type-error () (abort-with-code 422))))

(defun get-handler (params)
  (dequeue-schema-bind params
    (dequeue *db* :visibility-timeout visibility-timeout)))

(defun patch-handler (params)
  )

(defun delete-handler (params)
  )

(defun post-handler (params)
  )

(defun method-handler ()
  (with-request params
    (case (getf *request* :request-method)
      (:GET (get-handler params))
      (:DELETE (delete-handler params))
      (:PATCH (patch-handler params))
      (:POST (post-handler params))
      (t (abort-with-code 405)))))


(defun request-handler (*request*)
  (handler-case 
    (let ((response (method-handler)))
      (if response
          (content response)
          (empty 204)))
    (request-error (c) (empty (status-code c)))))

(defun start ()
  (let ((*db* (make-database)))
    (woo:run #'request-handler)))
