(ql:quickload :dexador)

(defvar *produced*)
(defvar *consumed*)

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


(defun consumer (target counter)
  (do ()
      ((>= (car counter) target))
      (multiple-value-bind (resp code)
        (dexador:get "http://localhost:5000/queue?visibility-timeout=86400"
                     :use-connection-pool nil)
        (when (= 200 code)
          (sb-ext:atomic-incf (car counter)) 
          (let ((content (read-from-string resp)))
            (destructuring-bind (&key producer message) content
              (setf (aref *consumed* producer message) content)))))))


(defun simple-sequential-test (&optional (count 10))
  (let ((*consumed* (make-array (list 1 count)))
        (*produced* (make-array (list 1 count)))
        (counter (list 0)))
    (producer 0 count)
    (consumer count counter)
    (print *consumed*)
    nil))

(defun multi-threaded-test (&key (count 100) (producers 10) (consumers 10))
  (let* ((*produced* (make-array (list producers count)))
         (*consumed* (make-array (list producers count)))
         (counter (list 0))
         (threads (append (loop for x from 0 to (1- producers) collect
                                (sb-thread:make-thread 
                                  (lambda (id *produced*)
                                    (producer id count))
                                  :arguments (list x *produced*)))
                          (loop for x from 0 to (1- consumers) collect
                                (sb-thread:make-thread 
                                  (lambda (*consumed*)
                                    (consumer (* count producers)
                                              counter))
                                  :arguments (list *consumed*))))))
    (loop for thread in threads do (sb-thread:join-thread thread))
    nil))
