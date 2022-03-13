(in-package #:cl-sqs)

(defvar *request*)
(defvar *db*)

(defconstantsafe +path+ "/queue")
(defconstantsafe +json-content+ "application/json")
(defconstantsafe +max-payload-size+ 65535)

(define-condition request-error (error)
  ((status-code :initarg :status-code :accessor status-code
                :type fixnum)))

(defun make-success (content)
  `(200 (:content-type "text/plain") ,(list content)))

(defun make-empty ()
  '(204 (:content-type "text/plain") ("")))

(defun make-error (code)
  `(,code (:content-type "text/plain") ("")))

(defun abort-with-code (code)
  (error 'request-error :status-code code))

(defmacro with-request ((params &key (path +path+) content-type)
                        &body body)
  `(handler-case
     (destructuring-bind (&key ,@params) (getf *request* :params)
       (
        (unless (equal ,path (getf *request* :path-info))
          (abort-with-code 404))
        (unless (equal ,content-type (getf *request* :content-type))
          (abort-with-code 406))
        ,@body))
     (sb-kernel::arg-count-error () (abort-with-code 422))
     (type-error () (abort-with-code 422))))

(defun get-handler ()
  (with-request ((visibility-timeout "60"))
    (execute *db* (parse-integer visibility-timeout))))

(defun patch-handler ()
  (with-request (visibility-timeout) 
    (execute *db* (parse-integer visibility-timeout))))

(defun delete-handler ()
  (with-request (id) 
    (execute *db* id)))

(defun post-handler ()
  (with-request (&key deduplication-id visibility-timeout
                      retention-timeout)
    (execute *db* (parse-integer visibility-timeout))
    ))

(defun method-handler ()
  (case (getf *request* :request-method)
    (:GET (get-handler))
    (:DELETE (delete-handler))
    (:PATCH (patch-handler))
    (:POST (post-handler))
    (t (abort-with-code 405))))


(defun request-handler (*request*)
  (handler-case 
    (progn
      (setf (getf *request* :params))
      (let ((response (method-handler)))
        (if response
            (make-success response)
            (make-empty))))
    (request-error (c) (make-error (status-code c)))))


(defun start ()
  (woo:run #'request-handler))

(defmethod b ((x (integer 1 2))) x)
(defun a (x) x)
(destructuring-bind (x) '("a")
  (declare (x (integer 0 10)))
  x)

(a)

