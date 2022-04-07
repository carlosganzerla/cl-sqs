(in-package #:cl-sqs)

(defvar *request*)

(defparameter *db* (make-database
                     :database (try-get-env "DB_NAME" "postgres")
                     :user (try-get-env "DB_USER" "postgres")
                     :password (try-get-env "DB_PASSWORD" "postgres")
                     :host (try-get-env "DB_HOST" "localhost")
                     :port (parse-integer (try-get-env "DB_PORT" "5432")))) 

(defconstantsafe +path+ "/queue")
(defconstantsafe +max-payload-size+ 65535)
(defconstantsafe +content-type+ "text/plain")

(defun read-payload (body content-length)
  (let ((octets (make-array content-length)))
    (when content-length (read-sequence octets body))
    (flexi-streams:octets-to-string octets)))

(defun url-decode (url)
  "Decode an encoded URL into a string."
  (with-output-to-string (s)
    (with-input-from-string (i url)
      (do ((c (read-char i nil nil)
              (read-char i nil nil)))
          ((null c))
          (case c
            (#\+ (princ #\space s))

            ;; 2-digit escaped ascii code
            (#\% (let ((c1 (read-char i nil nil))
                       (c2 (read-char i nil nil)))
                   (when (and c1 c2)
                     (let ((n1 (parse-integer (string c1) :radix 16))
                           (n2 (parse-integer (string c2) :radix 16)))
                       (princ (code-char (logior (ash n1 4) n2)) s)))))

            ;; just a normal character
            (otherwise (write-char c s)))))))

(defun parse-qs (qs)
  "Return an prop list of query string parameters."
  (when (and qs (> (length qs) 0))
    (loop
      with p = 0
      ;; find all the k/v pairs
      for n = (position #\& qs :start p)
      for m = (position #\= qs :start p :end n)

      ;; join all the pairs into an prop-list, decode values
      nconc (if m
                (let ((v (url-decode (subseq qs (1+ m) n))))
                  (list (str-to-kw (subseq qs p m)) v))
                (list (str-to-kw (subseq qs p n)) nil))
      ;; stop when no more keys
      while n
      ;; offset to the next k/v pair
      do (setf p (1+ n)))))


(defun response (code &optional (content "") &rest headers)
  `(,code (:content-type ,+content-type+ ,@headers) ,(list content)))

(defmacro with-request (params &body body)
  `(handler-case
     (let ((,params (parse-qs (getf *request* :query-string)))) 
       (if (equal +path+ (getf *request* :path-info))
           (progn ,@body)
           (response 404)))
     #+sbcl
     (sb-kernel::defmacro-lambda-list-bind-error () (response 422))
     (type-error () (response 422))
     (parse-error () (response 422))))

(defmacro with-response (params plist &body body)
  (let ((result (gensym)))
    `(let ((,result ,plist))
       (if ,result
           (destructuring-bind (&key ,@params) ,result
             ,@body)
           (response 204)))))

(defun get-handler (params)
  (dequeue-schema-bind params
    (with-response (payload message-timestamp)
                   (dequeue *db* :visibility-timeout visibility-timeout)
                   (response 200 payload
                             :message-timestamp message-timestamp))))

(defun post-handler (params)
  (let ((payload (read-payload (getf *request* :raw-body)
                               (min +max-payload-size+
                                    (getf *request* :content-length)))))
    (enqueue-schema-bind params
      (with-response (message-md5 message-timestamp)
                     (enqueue *db* payload :deduplication-id deduplication-id
                              :visibility-timeout visibility-timeout
                              :retention-timeout retention-timeout)
                     (response 201 ""
                               :message-timestamp message-timestamp
                               :message-md5 message-md5)))))

(defun request-handler (*request*)
  (print *request*)
  (with-slots (last-activity) (getf *request* :clack.io)
    (print last-activity))
  (return-from request-handler (response 200 "hello"))
  (with-request params
    (case (getf *request* :request-method)
      (:GET (get-handler params))
      (:POST (post-handler params))
      (t (response 405)))))

(defun start ()
  (woo:run #'request-handler :address "0.0.0.0"
           :worker-num 4))
