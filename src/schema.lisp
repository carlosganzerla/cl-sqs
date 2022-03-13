(in-package :cl-sqs)

(defconstantsafe +visibility-default+ 60)
(defconstantsafe +retention-default+ 24) 

(deftype visibility-timeout-t ()
  `(integer 0 86400))

(deftype retention-timeout-t ()
  `(integer 1 336))


(defstruct dequeue-params
  (visibility-timeout +visibility-default+
                      :type visibility-timeout-t
                      :read-only t))
