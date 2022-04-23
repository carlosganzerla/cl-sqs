(asdf:defsystem "cl-sqs-test"
  :version "0.1.0"
  :author "Carlo Sganzerla"
  :license "MIT"
  :depends-on (#:dexador #:cl-sqs)
  :pathname "t/"
  :components ((:file "package")
               (:file "client")
               (:file "concurrency")
               (:file "speed"))
  :description "Tests and benchmarks for cl-sqs")
