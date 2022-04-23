(in-package #:cl-sqs-test)

(defvar *consumed*)
(defparameter *db* (cl-sqs::make-database))


(defun producer (id count)
  (dotimes (x count)
    (unless (enqueue (random count) (list :message x :producer id))
      (error "Test failed"))))

(defun consumer (target)
  (do ()
      ((>= (length (car *consumed*)) target))
      (multiple-value-bind (payload receipt-id) (dequeue)
        (when payload
          (sb-ext:atomic-push payload (car *consumed*))
          (delete-message receipt-id)))))

(defun clean-queue ()
  (cl-sqs::with-database 
    (postmodern:query "DELETE FROM message")))

(defun check-results ()
  (cl-sqs::with-database
    (assert (postmodern:query "SELECT NOT EXISTS (SELECT * FROM message)"
                              :single)))
  (let ((consumed (apply #'vector *consumed*)))
    (dotimes (i (length consumed))
      (dotimes (j (length consumed))
        (when (/= i j)
          (assert (not (equal (aref consumed i) (aref consumed j)))))))))


(defun multi-threaded-test (&key (count 100) (producers 10) (consumers 10))
  "Check if concurrent producers/consumers sustain a consistent workflow"
  (clean-queue)
  (let* ((*consumed* (list nil))
         (threads (append (loop for x from 0 to (1- producers) collect
                                (sb-thread:make-thread 
                                  (lambda (id)
                                    (producer id count))
                                  :arguments (list x)))
                          (loop for x from 0 to (1- consumers) collect
                                (sb-thread:make-thread 
                                  (lambda (*consumed*)
                                    (consumer (* count producers)))
                                  :arguments (list *consumed*))))))
    (loop for thread in threads do (sb-thread:join-thread thread))
    (check-results)))
