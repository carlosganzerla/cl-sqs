(in-package :cl-sqs)

(defvar *with-connection-params* 
  '("postgres" "postgres" "postgres" "localhost" :pooled-p t))

(defmacro with-pgsql (&body body)
  `(postmodern:with-connection *with-connection-params* ,@body))

(defmacro query (&rest args)
  `(with-pgsql (postmodern:query ,@args)))


(defun enqueue (payload &optional (deduplication-id :NULL)
                        (visibility-timeout *visibility-timeout*)
                        (retention-timeout *retention-timeout*))
  (query "INSERT INTO
          queue (
                 payload,
                 deduplication_id,
                 visible_at,
                 expires_at
                 )
          VALUES (
                  $1::text,
                  COALESCE($2, encode(sha256($1::bytea), 'base64')),
                  NOW() + $3 * INTERVAL '1 SECOND',
                  NOW() + $4 * INTERVAL '1 HOUR'
                  )
          ON CONFLICT (deduplication_id)
          DO NOTHING
          RETURNING 
          json_build_object
          (
           'id', queue.id,
           'payload_hash', md5(payload)
           );" 
         payload deduplication-id
         visibility-timeout retention-timeout :single))

