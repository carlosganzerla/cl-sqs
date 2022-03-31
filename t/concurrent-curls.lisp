(defparameter *curl*
  "curl -o /dev/null -s -w 'Total: %{time_total}s\\n' \\
   http://localhost:5000/queue")

(defun concurrent-curls (count)
  (dotimes (_ count)
    (uiop:launch-program *curl* :output *standard-output*)))

(concurrent-curls 1000)
