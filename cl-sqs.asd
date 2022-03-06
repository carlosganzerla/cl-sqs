(defsystem "cl-sqs"
  :version "0.1.0"
  :author "Carlo Sganzerla"
  :license "MIT"
  :depends-on (#:woo #:flexi-streams)
  :pathname "src/"
  :components ((:file "package")
               (:file "utils")
               (:file "queue")
               (:file "server"))
  :description "A lightweight self contained SQS-like queue")
