(in-package #:cl-sqs)

(defun enqueue (payload)
  (make-success payload))

(defun dequeue ()
  (make-success "Hello world"))
