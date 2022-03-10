(in-package :cl-sqs)

(defconstantsafe +enqueue+ (read-file #p"db/queries/enqueue.sql"))
(defconstantsafe +dequeue+ (read-file #p"db/queries/dequeue.sql"))
(defconstantsafe +change-visibility+
             (read-file #p"db/queries/change_visibility.sql"))
(defconstantsafe +delete+ (read-file #p"db/queries/delete.sql"))


(defvar *with-connection-params* 
  '("postgres" "postgres" "postgres" "localhost" :pooled-p t))

(defmacro with-pgsql (&body body)
  `(postmodern:with-connection *with-connection-params* ,@body))

(defmacro query (&rest args)
  `(with-pgsql (postmodern:query ,@args)))


(defun enqueue (payload &key deduplication-id
                        visibility-timeout
                        retention-timeout)
  (query +enqueue+ payload deduplication-id visibility-timeout 
         retention-timeout :single))

(defun dequeue (&key visibility-timeout)
  (query +dequeue+ visibility-timeout :single))

(defun change-visibility (id timeout)
  (query +change-visibility+ id timeout))

(defun delete-message (id)
  (query +delete+ id))
