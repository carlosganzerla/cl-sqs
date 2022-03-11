(in-package #:cl-sqs)

(defvar *request*)

(defvar *db*)

(defconstantsafe +path+ "/queue")
(defconstantsafe +json-content+ "application/json")
(defconstantsafe +max-payload-size+ 65535)
(defconstantsafe +max-url-size+ 200)

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

(defun read-payload ()
  (let* ((len (min +max-payload-size+ (getf *request* :content-length)))
         (octets (make-array len)))
    (when len
      (read-sequence octets (getf *request* :raw-body)))
    (flexi-streams:octets-to-string octets)))

(defun validate-params (schema)
  (let* ((raw-qs (getf *request* :query-string))
         (qs (subseq raw-qs 0 (min +max-url-size+ (length raw-qs)))))
    (handler-case
      (make-success (format nil "~A" (apply schema (parse-query-string qs))))
      (error (e)
             (progn 
               (format t "~A" e)
               (make-error 422))))))

(defun validate-content-type (content-type)
  (unless (equal content-type (getf *request* :content-type))
    (abort-with-code 406)) 
  t)

(defun validate-path (path)
  (unless (equal path (getf *request* :path-info))
    (abort-with-code 404))
  t)

(defun get-handler ()
  (and (validate-content-type nil) 
       (validate-path +path+)
       (validate-params #'dequeue-params-schema)))

(defun patch-handler ()
  (and (validate-content-type nil)
       (validate-path +path+)
       (validate-params #'change-visibility-schema)))

(defun delete-handler ()
  (and (validate-content-type nil) 
       (validate-path +path+)
       (validate-params #'delete-message-schema)))

(defun post-handler ()
  (and (validate-content-type +json-content+)
       (validate-path +path+)
       (validate-params #'enqueue-schema)))

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
    (request-error (c) (make-error (status-code c)))))


(defun start ()
  (woo:run #'request-handler))
