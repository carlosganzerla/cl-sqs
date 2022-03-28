(ql:quickload :dexador)
(ql:quickload :postmodern)
(ql:quickload :cl-sqs)

(defvar *produced*)
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
          (error "Test failed"))
        (setf (aref *produced* id x) content)))))


(defun consumer (target results)
  (do ()
      ((>= (length (car results)) target))
      (multiple-value-bind (resp code)
        (dexador:get "http://localhost:5000/queue?visibility-timeout=86400"
                     :use-connection-pool nil)
        (when (= 200 code)
          (let ((content (read-from-string resp)))
            (sb-ext:atomic-push content (car results))
            (destructuring-bind (&key producer message) content
              (setf (aref *consumed* producer message) content)))))))


(defun simple-sequential-test (&optional (count 10))
  (let ((*consumed* (make-array (list 1 count)))
        (*produced* (make-array (list 1 count)))
        (results (list nil)))
    (producer 1 count)
    (consumer count results)
    (assert (equal (select-results) (nreverse (car results))))
    nil))


(defun select-results ()
  (mapcar #'read-from-string
          (cl-sqs::with-database *db*
            (postmodern:query "SELECT payload FROM queue ORDER BY id"
                              :column))))
(defun multi-threaded-test1 (&key (count 100) (producers 10) (consumers 10))
  (let* ((*produced* (make-array (list producers count)))
         (*consumed* (make-array (list producers count)))
         (results (list nil))
         (producer-threads (loop for x from 0 to (1- producers) collect
                                 (sb-thread:make-thread 
                                   (lambda (id *produced*)
                                     (producer id count))
                                   :arguments (list x *produced*))))
         (consumer-threads (loop for x from 0 to (1- consumers) collect
                                 (sb-thread:make-thread 
                                   (lambda (*consumed*)
                                     (consumer (* count producers)
                                               results))
                                   :arguments (list *consumed*)))))
    (loop for thread in producer-threads do (sb-thread:join-thread thread))
    (loop for thread in consumer-threads do (sb-thread:join-thread thread))
    results))

(defun multi-threaded-test2 (&key (count 100) (producers 10) (consumers 10))
  (let* ((*produced* (make-array (list producers count)))
         (*consumed* (make-array (list producers count)))
         (results (list nil))
         (threads (append (loop for x from 0 to (1- producers) collect
                                (sb-thread:make-thread 
                                  (lambda (id *produced*)
                                    (producer id count))
                                  :arguments (list x *produced*)))
                          (loop for x from 0 to (1- consumers) collect
                                (sb-thread:make-thread 
                                  (lambda (*consumed*)
                                    (consumer (* count producers)
                                              results))
                                  :arguments (list *consumed*))))))
    (loop for thread in threads do (sb-thread:join-thread thread))
    results))
