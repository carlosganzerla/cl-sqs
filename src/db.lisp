(in-package #:cl-sqs)

(defstruct database
  (database "postgres" :type string :read-only t)
  (user "postgres" :type string :read-only t)
  (password "postgres" :type string :read-only t)
  (host "localhost" :type string :read-only t)
  (port 5432 :type integer :read-only t))

(defmacro with-database (&body body)
  `(with-slots (database host port user password pooled-p) *db*
     (postmodern:with-connection  (list database user password host
                                        :port port
                                        :pooled-p t)
       ,@body)))

(defmacro query (db &rest args)
  `(with-database 
     (let ((results (postmodern:query ,@args :array-hash))
           (headers))
       (if (> (length results) 0)
           (progn
             (maphash (lambda (k v)
                        (unless (string= k "payload")
                          (push (str-to-kw k) headers)
                          (push v headers))) 
                      (aref results 0))
             (values (or (gethash "payload" (aref results 0)) "")
                     (nreverse headers)))
           (values nil nil)))))


(defun enqueue (group-id payload deduplication-id)
  (query *db* (read-file-memo #p"db/queries/enqueue.sql")
         group-id payload deduplication-id))

(defun dequeue (visibility-timeout)
  (query *db* (read-file-memo #p"db/queries/dequeue.sql") visibility-timeout))

(defun change-visibility (receipt-id timeout)
  (query *db* (read-file-memo #p"db/queries/change_visibility.sql") 
         receipt-id timeout))

(defun delete-message (receipt-id)
  (query *db* (read-file-memo #p"db/queries/delete.sql")
         receipt-id))
