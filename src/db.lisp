(in-package :cl-sqs)

(defstruct database
  (database "postgres" :type string :read-only t)
  (user "postgres" :type string :read-only t)
  (password "postgres" :type string :read-only t)
  (host "postgres" :type string :read-only t)
  (port 5432 :type integer :read-only t))

(defmacro with-database (db &body body)
  `(with-slots (database host port user password pooled-p)
    (postmodern:with-connection  (list database user password host
                                       :port port
                                       :pooled-p t)
      ,@body)))

(defmacro query (db &rest args)
  `(with-database db (postmodern:query ,@args)))


(defmethod enqueue ((db database) payload &key deduplication-id
                                  visibility-timeout
                                  retention-timeout)
  (query db (read-file-lazy #p"db/queries/enqueue.sql")
         payload deduplication-id visibility-timeout 
         retention-timeout :single))

(defmethod dequeue ((db database) &key visibility-timeout)
  (query db (read-file-lazy #p"db/queries/dequeue.sql")
         visibility-timeout :single))

(defmethod change-visibility ((db database) id timeout)
  (query db (read-file #p"db/queries/change_visibility.sql")
         id timeout))

(defmethod delete-message ((db database) id)
  (query db (read-file #p"db/queries/delete.sql") id))
