(in-package #:cl-sqs)

(defvar *request*)

(defvar *allowed-contents* '("application/json"))

(defvar *allowed-methods* '(:GET :POST))

(defvar *allowed-paths* '("/queue"))


(defun request-handler (*request*)
  (with-proplist-items (path-info request-method content-type) *request*
    (print *request*)
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

(defun make-success (content)
  `(200 (:content-type "application/json") ,(list content)))

(defun make-error (code)
  `(,code (:content-type "text/plain") ("")))

(defun start ()
  (woo:run #'request-handler))
