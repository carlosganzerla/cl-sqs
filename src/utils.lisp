(in-package #:cl-sqs)

(defun symbol-to-kw (symbol)
  (intern (symbol-name symbol) :keyword))

(defun str-to-kw (str)
  (intern (string-upcase str) :keyword))

(let ((read-files (make-hash-table)))
  (defun read-file-lazy (filename)
    (if (gethash filename read-files)
        (gethash filename read-files)
        (with-open-file (stream filename)
          (let ((contents (make-string (file-length stream))))
            (read-sequence contents stream)
            (setf (gethash filename read-files) contents)
            contents)))))

(defmacro defconstantsafe (sym val)
  `(unless (boundp ',sym)
     (defconstant ,sym ,val)))

(defun tc (val type)
  (if (typep val type)
      val
      (error 'simple-type-error
             :datum val
             :expected-type type
             :format-control
             "Type ~A not satisfied. Datum: ~A"
             :format-arguments (list type val))))

(defun car-if-list (x)
  (if (listp x)
      (car x)
      x))

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
