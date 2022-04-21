(ql:quickload :dexador)
(ql:quickload :postmodern)
(ql:quickload :cl-sqs)

(defvar *consumed*)
(defparameter *db* (cl-sqs::make-database))


(defun producer (id count)
  (dotimes (x count)
    (let ((content (list :message x :producer id)))
      (multiple-value-bind (resp code)
        (dexador:post 
          (format nil "http://localhost:5000/queue?message-group-id=~A"
                  (random count))
          :content (write-to-string content)
          :use-connection-pool nil)
        (when (/= code 200)
          (format t "Response ~A, code ~A" resp code)
          (error "Test failed"))))))

(defun consumer (target)
  (do ()
      ((>= (length (car *consumed*)) target))
      (multiple-value-bind (resp code headers)
        (dexador:get "http://localhost:5000/queue?visibility-timeout=86400"
                     :use-connection-pool nil)
        (when (= 200 code)
          (sb-ext:atomic-push resp (car *consumed*))
          (dexador:delete
            (format nil "http://localhost:5000/queue?message-receipt-id=~A"
                    (gethash "message-receipt-id" headers)))))))

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
