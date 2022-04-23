(in-package #:cl-sqs-test)

(defun enqueue (group-id payload)
  (multiple-value-bind (resp code headers)
    (dexador:post
      (format nil "http://localhost:5000/queue?message-group-id=~A"
              group-id)
      :content (write-to-string payload)
      :use-connection-pool nil)
    (declare (ignore resp headers))
    (cond ((= code 204) nil)
          ((= code 200) t)
          (t (error "Could not enqueue: code ~A" code)))))

(defun dequeue (&optional (visibility-timeout 86400))
  (multiple-value-bind (resp code headers)
    (dexador:get "http://localhost:5000/queue?visibility-timeout=86400"
                 :use-connection-pool nil)
    (cond ((= code 204) nil)
          ((= code 200) (values (read-from-string resp)
                                (gethash "message-receipt-id" headers)))
          (t (error "Could not dequeue: code ~A" code)))))

(defun delete-message (receipt-id)
  (multiple-value-bind (resp code headers)
    (dexador:delete
      (format nil "http://localhost:5000/queue?message-receipt-id=~A"
              receipt-id)
      :use-connection-pool nil)
    (declare (ignore resp headers))
    (cond ((= code 204) nil)
          ((= code 200) t)
          (t (error "Could not delete: code ~A" code)))))
