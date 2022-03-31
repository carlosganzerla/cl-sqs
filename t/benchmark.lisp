(ql:quickload :dexador)
(ql:quickload :postmodern)
(ql:quickload :cl-sqs)

(defun insert-bulk (count)
  (cl-sqs::with-database *db*
    (postmodern:query
      (cl-sqs::read-file-memo #p"db/queries/benchmark.sql") count)))

(defun consume-messages (count)
  (let ((result nil))
    (dotimes (x count)
      (push
        (dexador:get "http://localhost:5000/queue?visibility-timeout=86400")
        result)
      result)))

(defun check-list (list)
  (do ((xs list (cdr xs)))
      ((not (cdr xs)))
      (assert (string/= (car xs) (cadr xs)))))

(defun benchmark (&key (rows 1000000) (msgs 500))
  (clean-queue)
  (insert-bulk rows)
  (let ((result nil))
    (time (setf result (consume-messages msgs)))
    (check-list (sort result #'string<))))

