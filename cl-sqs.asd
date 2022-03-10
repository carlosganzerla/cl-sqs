(defsystem "cl-sqs"
  :version "0.1.0"
  :author "Carlo Sganzerla"
  :license "MIT"
  :depends-on (#:woo #:flexi-streams #:postmodern)
  :pathname "src/"
  :components ((:file "package")
               (:file "utils")
               (:file "schema")
               (:file "db")
               (:file "server"))
  :description "A lightweight self contained SQS-like queue")
