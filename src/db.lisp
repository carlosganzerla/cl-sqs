(in-package :cl-sqs)

(defstruct database
  (database "postgres" :type string :read-only t)
  (user "postgres" :type string :read-only t)
  (password "postgres" :type string :read-only t)
  (host "localhost" :type string :read-only t)
  (port 5432 :type integer :read-only t))

(defmacro with-database (db &body body)
  `(with-slots (database host port user password pooled-p) ,db
     (postmodern:with-connection  (list database user password host
                                        :port port
                                        :pooled-p t)
       ,@body)))

(defmacro query (db &rest args)
  `(with-database ,db (postmodern:query ,@args :plist)))


(defmethod enqueue ((db database) payload &key deduplication-id
                                  visibility-timeout
                                  retention-timeout)
  (query db (read-file-lazy #p"db/queries/enqueue2.sql")
         payload deduplication-id visibility-timeout 
         retention-timeout))

(defmethod dequeue ((db database) &key visibility-timeout)
  (query db (read-file-lazy #p"db/queries/dequeue3.sql")
         visibility-timeout))

(defmethod change-visibility ((db database) id timeout)
  (query db (read-file-lazy #p"db/queries/change_visibility.sql")
         id timeout))

(defmethod delete-message ((db database) id)
  (query db (read-file-lazy #p"db/queries/delete.sql") id))
