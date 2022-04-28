(in-package #:cl-sqs)

(defparameter *queue* (sb-concurrency:make-queue))

(defun %log-data (args)
  (format t "[~A] ~A~%" (get-timestamp-string) (apply #'format nil args)))

(defun log-data (&rest args)
  (sb-concurrency:enqueue args *queue*))

(defun start-logger ()
  (sb-thread:make-thread
    (lambda (queue)
      (loop
        (let ((message (sb-concurrency:dequeue queue)))
          (when message
            (%log-data message))
          (sb-thread:thread-yield))))
    :name "Logger thread"
    :arguments *queue*))
