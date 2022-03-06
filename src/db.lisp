(in-package :cl-sqs)

(defvar *with-connection-params* 
  '("postgres" "postgres" "postgres" "localhost" :pooled-p t))

(defmacro with-pgsql (&body body)
  `(postmodern:with-connection *with-connection-params* ,@body))

(defmacro query (&rest args)
  `(with-pgsql (postmodern:query ,@args)))

(defconstant +enqueue+ (read-file #p"db/queries/enqueue.sql"))
(defconstant +dequeue+ (read-file #p"db/queries/dequeue.sql"))
(defconstant +change-visibility+
             (read-file #p"db/queries/change_visibility.sql"))
(defconstant +delete+ (read-file #p"db/queries/delete.sql"))


(defun enqueue (payload &key (deduplication-id :NULL)
                        (visibility-timeout 0)
                        (retention-timeout *retention-timeout*))
  (query +enqueue+ payload deduplication-id visibility-timeout 
         retention-timeout :single))

(defun dequeue (&key (visibility-timeout *visibility-timeout*))
  (query +dequeue+ visibility-timeout :single))

(defun change-visibility (id timeout)
  (query +change-visibility+ id timeout))

(defun delete-message (id)
  (query +delete+ id))

(enqueue "carlo" :deduplication-id "14" :visibility-timeout 120)
(dequeue)
(delete-message "8b6d27f2-0998-4a06-aaa6-5a2b93ea3364")
(change-visibility "3f708854-7d9d-4b1e-97b0-58b877081f3e" "300")
