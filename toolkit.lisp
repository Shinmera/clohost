(in-package #:org.shirakumo.clohost)

(defvar *unix-epoch-difference* (encode-universal-time 0 0 0 1 1 1970 0))
(defun universal->unix (universal)
  (- universal *unix-epoch-difference*))

(defun unix->universal (unix)
  (+ unix *unix-epoch-difference*))

(defun to-universal (thing)
  (etypecase thing
    (integer (unix->universal thing))
    (string (parse-timestring thing))))

(defun parse-timestring (string)
  (let* ((y -1)
         (x (position #\- string))
         (d (position #\- string :start (1+ x)))
         (h (position #\T string :start (1+ d)))
         (m (position #\: string :start (1+ h)))
         (s (position #\: string :start (1+ m)))
         (ms (position #\. string :start (1+ s))))
    (flet ((part (s e)
             (parse-integer string :start s :end e)))
      (encode-universal-time
       (part (1+ s) ms)
       (part (1+ m) s)
       (part (1+ h) m)
       (part (1+ d) h)
       (part (1+ x) d)
       (part (1+ y) x)
       0))))

(defun universal->utc-timestring (universal)
  (multiple-value-bind (seconds minutes hours day month year)
      (decode-universal-time universal 0)
    (format NIL
            "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hours minutes seconds)))

(defun from-bool (bool)
  (etypecase bool
    (integer (= 1 bool))
    (symbol bool)
    (string
     (or (string-equal bool "true")
         (string-equal bool "t")
         (string-equal bool "yes")
         (string-equal bool "allowed")
         (string-equal bool "1")))))

(defun to-keyword (thing)
  (intern (with-output-to-string (out)
            (loop with was-dash = T
                  for char across thing
                  do (if (upper-case-p char)
                         (unless was-dash
                           (write-char #\- out)
                           (setf was-dash T))
                         (setf was-dash NIL))
                     (if (char= #\_ char)
                         (write-char #\- out)
                         (write-char (char-upcase char) out))))
          "KEYWORD"))

(defun to-key (a)
  (with-output-to-string (out)
    (with-input-from-string (in (string a))
      (loop for char = (read-char in NIL)
            do (case char
                 ((NIL) (return))
                 (#\- (write-char (char-upcase (read-char in)) out))
                 (T (write-char (char-downcase char) out)))))))

(defun %getj (data &rest attributes)
  (cond ((null data) NIL)
        ((null attributes) data)
        (T (let ((attribute (first attributes)))
             (apply #'%getj
                    (etypecase attribute
                      (string (gethash attribute data))
                      (integer (when (< attribute (length data))
                                 (elt data attribute))))
                    (rest attributes))))))

(defun getj (data &rest attributes)
  (apply #'%getj data (loop for attribute in attributes
                            collect (etypecase attribute
                                      ((or number string) attribute)
                                      (keyword (to-key attribute))))))

(define-compiler-macro getj (&environment env data &rest attributes)
  (let ((gensym (gensym "ATTRIBUTE")))
    `(%getj ,data ,@(loop for attribute in attributes
                          for form = `(let ((,gensym ,attribute))
                                        (etypecase ,gensym
                                          ((or number string) ,gensym)
                                          (keyword (to-key ,gensym))))
                          collect (if (constantp attribute env)
                                      `(load-time-value ,form)
                                      form)))))

(defun tab (&rest args)
  (let ((table (make-hash-table :test 'equal)))
    (loop for (k v) on args by #'cddr
          when v
          do (setf (gethash (to-key k) table) v))
    table))

(defun tab* (existing &rest args)
  (let ((table (make-hash-table :test 'equal)))
    (loop for k being the hash-keys of existing using (hash-value v)
          do (setf (gethash k table) v))
    (loop for (k v) on args by #'cddr
          when v
          do (setf (gethash (to-key k) table) v))
    table))

(defun from-tab (tab)
  (etypecase tab
    (hash-table
     (loop for k being the hash-keys of tab using (hash-value v)
           collect (to-keyword k) collect v))
    (list
     (map 'list #'from-tab tab))
    (vector
     (map 'list #'from-tab tab))))

(defun starts-with (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun to-list (thing)
  (etypecase thing
    (list thing)
    (sequence (map 'list #'identity thing))))

(defun jsonify (params &rest args)
  (apply #'com.inuoe.jzon:stringify
         (loop with table = (make-hash-table :test 'equal)
               for (k v) on params by #'cddr
               unless (eql v :||)
               do (setf (gethash (to-key k) table) v)
               finally (return table))
         args))
