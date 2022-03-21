(defvar *produced*)

(defvar *failed*)

(defun producer (id count)
  (dotimes (x count)
    (let ((content (list :message x :producer id)))
      (setf (aref *produced* id x) content)
      (dexador:post "http://localhost:5000/queue"
                    :content (write-to-string content)))))


(defun consumer (max-times)
  (dotimes (x max-times (setf *failed* t))
    (multiple-value-bind (resp code)
      (dexador:get "http://localhost:5000/queue")
      (when (= 204 code)
        (return))
      ()


      )
    )

  )

(producer 1 10)

(princ '(:a 1) nil)


(make-array '(2 2) :initial-element nil)

(multiple-value-bind (res code headers)
  (dexador:get "http://localhost:5000/queue")
  (maphash (lambda (k v)
             (format t "~A ~A~%" k v))
           headers))
