(ql:quickload :dexador)
(defvar *produced*)

(defvar *consumed*)

(defun producer (id count)
  (dotimes (x count)
    (let ((content (list :message x :producer id)))
      (setf (aref *produced* id x) content)
      (multiple-value-bind (resp code)
        (dexador:post "http://localhost:5000/queue"
                      :content (write-to-string content)
                      :use-connection-pool nil)
        (when (/= code 201)
          (format t "Response ~A, code ~A" resp code)
          (error "Test failed"))))))


(defun consumer (id count)
  (dotimes (x count)
    (multiple-value-bind (resp code)
      (dexador:get "http://localhost:5000/queue?visibility-timeout=86400"
                   :use-connection-pool nil)
      (when (= 204 code) (return))
      (setf (aref *consumed* id x) (read-from-string resp)))))


(defun simple-sequential-test ()
  (let ((*produced* (make-array '(1 10)))
        (*consumed* (make-array '(1 10))))
    (producer 0 10)
    (consumer 0 10)
    (print *consumed*)
    (print *produced*)
    nil))

(defun multi-threaded-test (&key (count 100) (producers 10) (consumers 10))
  (let* ((*produced* (make-array (list producers count)))
         (*consumed* (make-array (list consumers count)))
         (threads (append (loop for x from 0 to (1- producers) collect
                                (sb-thread:make-thread 
                                  (lambda (id *produced*)
                                    (producer id count))
                                  :arguments (list x *produced*)))
                          (loop for x from 0 to (1- consumers) collect
                                (sb-thread:make-thread 
                                  (lambda (id *consumed*)
                                    (consumer id count))
                                  :arguments (list x *consumed*))))))
    (loop for thread in threads do (sb-thread:join-thread thread))
    nil))
