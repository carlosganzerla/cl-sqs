(in-package #:cl-sqs)

(defvar *request*)
(defvar *db*)


(defconstantsafe +path+ "/queue")
(defconstantsafe +max-payload-size+ 65535)
(defconstantsafe +content-type+ "application/json")

(define-condition request-error (error)
  ((status-code :initarg :status-code :accessor status-code
                :type fixnum)))


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


(defun content (content &rest headers)
  `(200 (:content-type ,+content-type+) ,(list content)))

(defun empty (code)
  `(,code (:content-type ,+content-type+) ("")))

(defun abort-with-code (code)
  (error 'request-error :status-code code))

(defmacro with-request (params &body body)
  `(handler-case
     (let ((,params (parse-qs (getf *request* :query-string)))) 
       (unless (equal +path+ (getf *request* :path-info))
         (abort-with-code 404))
       ,@body)
     #+sbcl
     (sb-kernel::defmacro-lambda-list-bind-error () (abort-with-code 422))
     (type-error () (abort-with-code 422))
     (parse-error () (abort-with-code 422))))


(defun get-handler (params)
  (dequeue-schema-bind params
    (dequeue *db* :visibility-timeout visibility-timeout)))

(defun post-handler (params)
  (let ((payload (read-payload (getf *request* :raw-body)
                               (min +max-payload-size+
                                    (getf *request* :content-length)))))
    (enqueue-schema-bind params
      (enqueue *db* payload :deduplication-id deduplication-id
               :visibility-timeout visibility-timeout
               :retention-timeout retention-timeout))))

(defun method-handler ()
  (with-request params
    (case (getf *request* :request-method)
      (:GET (get-handler params))
      (:POST (post-handler params))
      (t (abort-with-code 405)))))


(defun request-handler (*request*)
  (handler-case 
    (let ((response (method-handler)))
      (if response
          (content response)
          (empty 204)))
    (request-error (c) (empty (status-code c)))))

(defun start ()
  (let ((*db* (make-database)))
    (woo:run #'request-handler :address "0.0.0.0")))
