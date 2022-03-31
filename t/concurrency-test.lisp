(ql:quickload :dexador)
(ql:quickload :postmodern)
(ql:quickload :cl-sqs)

(defvar *consumed*)
(defparameter *db* (cl-sqs::make-database))

(defun producer (id count)
  (dotimes (x count)
    (let ((content (list :message x :producer id)))
      (multiple-value-bind (resp code)
        (dexador:post "http://localhost:5000/queue"
                      :content (write-to-string content)
                      :use-connection-pool nil)
        (when (/= code 201)
          (format t "Response ~A, code ~A" resp code)
          (error "Test failed"))))))

(defun consumer (target)
  (do ()
      ((>= (length (car *consumed*)) target))
      (multiple-value-bind (resp code headers)
        (dexador:get "http://localhost:5000/queue?visibility-timeout=86400"
                     :use-connection-pool nil)
        (when (= 200 code)
          (sb-ext:atomic-push
            (list :payload resp
                  :timestamp (parse-integer (gethash "message-timestamp"
                                                     headers)))
            (car *consumed*))))))

(defun consumed-results ()
  (sort (reverse (car *consumed*))
        (lambda (c1 c2)
          (< (getf c1 :timestamp) (getf c2 :timestamp)))))

(defun select-results ()
  (cl-sqs::with-database *db*
    (postmodern:query
      "SELECT payload,
       (extract(EPOCH from queue.visible_at) * 1000000) timestamp
       FROM
       queue
       ORDER BY visible_at" :plists)))

(defun clean-queue ()
  (cl-sqs::with-database *db*
    (postmodern:query "DELETE FROM queue")))

(defun simple-sequential-test (&optional (count 10))
  (clean-queue)
  (let ((*consumed* (list nil)))
    (producer 0 count)
    (consumer count)
    (assert (equal (select-results) (consumed-results)))
    nil))



(defun multi-threaded-test1 (&key (count 100) (producers 10) (consumers 10))
  (clean-queue)
  (let* ((*consumed* (list nil))
         (producer-threads (loop for x from 0 to (1- producers) collect
                                 (sb-thread:make-thread 
                                   (lambda (id)
                                     (producer id count))
                                   :arguments (list x))))
         (consumer-threads (loop for x from 0 to (1- consumers) collect
                                 (sb-thread:make-thread 
                                   (lambda (*consumed*)
                                     (consumer (* count producers)))
                                   :arguments (list *consumed*)))))
    (loop for thread in producer-threads do (sb-thread:join-thread thread))
    (loop for thread in consumer-threads do (sb-thread:join-thread thread))
    (assert (equal (select-results) (consumed-results)))))

(defun multi-threaded-test2 (&key (count 100) (producers 10) (consumers 10))
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
    (assert (equal (select-results) (consumed-results)))))
