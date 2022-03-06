(defvar *request*)
(defparameter *allowed-contents* '("application/json"))

(defparameter *allowed-methods* '(:GET :POST))

(defparameter *allowed-paths* '("/"))

(defun symbol-to-kw (symbol)
  (intern (symbol-name symbol) :keyword))

(defmacro with-proplist-items (slots place &body body)
  `(let (,@(mapcar (lambda (symbol)
                     `(,symbol (getf ,place ,(symbol-to-kw symbol))))
                   slots))
     ,@body))

(defun request-handler (*request*)
  (with-proplist-items (path-info request-method content-type) *request*
    (if (and (member request-method *allowed-methods*)
             (member path-info *allowed-paths* :test #'string=)
             (or (member content-type *allowed-contents* :test #'string=)
                 (and (eql request-method :GET) (not content-type))))
        (case request-method
          (:POST (enqueue (read-payload)))
          (:GET (dequeue))
          (t (make-error 405)))
        (make-error 400))))

(defun read-payload ()
  (let* ((len (getf *request* :content-length))
         (octets (make-array len)))
    (when len
      (read-sequence octets (getf *request* :raw-body)))
    (flexi-streams:octets-to-string octets)))

(defun enqueue (payload)
  (make-success payload))

(defun dequeue ()
  (make-success "hello world"))

(defun make-success (content)
  `(200 (:content-type "application/json") ,(list content)))

(defun make-error (code)
  `(,code (:content-type "text/plain") ("")))

(woo:run #'request-handler)
