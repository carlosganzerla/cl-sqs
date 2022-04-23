(in-package #:cl-sqs)

(defvar *request*)
(defparameter *api-key* (sb-ext:posix-getenv "API_KEY"))

(defparameter *db* (make-database
                     :database (try-get-env "DB_NAME" "postgres")
                     :user (try-get-env "DB_USER" "postgres")
                     :password (try-get-env "DB_PASSWORD" "postgres")
                     :host (try-get-env "DB_HOST" "localhost")
                     :port (parse-integer (try-get-env "DB_PORT" "5432")))) 

(defconstantsafe +path+ "/queue")
(defconstantsafe +max-payload-size+ 65535)
(defconstantsafe +content-type+ "text/plain")

(defun get-header (header)
  (gethash header (getf *request* :headers)))

(defun read-payload ()
  (let* ((content-length (min +max-payload-size+
                             (getf *request* :content-length)))
        (octets (make-array content-length)))
    (read-sequence octets (getf *request* :raw-body))
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
  (destructuring-bind (&key request-method content-length request-uri
                            &allow-other-keys) *request*
    (log-data "~A - ~A ~A (~A bytes)" code request-method request-uri
              (or content-length 0) ))
  `(,code (:content-type ,+content-type+ ,@headers) ,(list content)))

(defmacro with-request ((params) &body body)
  `(block nil
          (handler-case
            (let ((,params (parse-qs (getf *request* :query-string)))) 
              (unless (equal +path+ (getf *request* :path-info))
                (return (response 404)))
              (unless (or (not *api-key*)
                          (string= *api-key* (get-header "api-key")))
                (return (response 401)))
              (progn ,@body))
            #+sbcl
            (sb-kernel::defmacro-lambda-list-bind-error () (response 422))
            (type-error () (response 422))
            (parse-error () (response 422))
            (error (e) ()
                   (log-data "ERROR: ~A" e)
                   (response 500)))))

(defmacro with-response (form)
  (let ((result (gensym))
        (headers (gensym)))
    `(multiple-value-bind (,result ,headers) ,form
       (if ,result
           (apply #'response 200 ,result ,headers)
           (response 204)))))

(defun get-handler (params)
  (dequeue-schema-bind params (visibility-timeout)
    (with-response (dequeue visibility-timeout))))

(defun post-handler (params)
  (let ((payload (read-payload)))
    (enqueue-schema-bind params (deduplication-id message-group-id)
      (with-response
        (enqueue message-group-id payload deduplication-id)))))

(defun patch-handler (params)
  (change-visibility-schema-bind params (message-receipt-id visibility-timeout)
    (with-response (change-visibility message-receipt-id visibility-timeout))))

(defun delete-handler (params)
  (delete-schema-bind params (message-receipt-id)
    (with-response (delete-message message-receipt-id))))

(defun request-handler (*request*)
  (with-request (params)
    (case (getf *request* :request-method)
      (:GET (get-handler params))
      (:POST (post-handler params))
      (:PATCH (patch-handler params))
      (:DELETE (delete-handler params))
      (t (response 405)))))

(defun start ()
  (apply #'woo:run #'request-handler 
         (let ((args (sb-ext:posix-getenv "WOO_ARGS")))
           (if args
               (read-from-string args)
               (list :address "0.0.0.0" :port 80 :debug nil :worker-num 4)))))
