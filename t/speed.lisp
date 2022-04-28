(in-package #:cl-sqs-test)

(defun insert-bulk (count)
  (cl-sqs::with-database *db*
    (postmodern:execute
      (cl-sqs::read-file-memo #p"db/queries/benchmark.sql")
      (max 1 (/ count 100)) count)))


(defun database-benchmark (&key (rows 1000000))
  "Benchmarks database query (speed of enqueueing, dequeuing and deleting after
   a large number of messages are present)"
  (clean-queue)
  (insert-bulk rows)
  (let ((msg nil))
    (time (assert (enqueue (random (/ rows 100)) "hi")))
    (time (multiple-value-bind (resp receipt-id) (dequeue)
            (assert resp)
            (print resp)
            (setf msg receipt-id)))
    (time (assert (delete-message msg)))))
