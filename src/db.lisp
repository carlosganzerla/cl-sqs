(in-package :cl-sqs)

(defvar *with-connection-params* 
  '("postgres" "postgres" "postgres" "localhost" :pooled-p t))

(defmacro with-pgsql (&body body)
  `(postmodern:with-connection *with-connection-params* ,@body))

(defmacro query (&rest args)
  `(with-pgsql (postmodern:query ,@args)))



(defun enqueue (payload deduplication-id visibility-timeout
                        retention-timeout))

(query "SELECT
        payload
        FROM
        queue
        " :single)
