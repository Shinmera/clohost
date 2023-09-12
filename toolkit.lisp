(in-package #:org.shirakumo.clohost)

(defvar *unix-epoch-difference* (encode-universal-time 0 0 0 1 1 1970 0))
(defun universal->unix (universal)
  (- universal *unix-epoch-difference*))

(defun unix->universal (unix)
  (+ unix *unix-epoch-difference*))

(defun to-keyword (thing)
  (intern (with-output-to-string (out)
            (loop for char across thing
                  do (if (char= #\_ char)
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
  (if (null attributes)
      data
      (let ((attribute (first attributes)))
        (apply #'getj
               (etypecase attribute
                 (string (gethash attribute data))
                 (integer (elt data attribute)))
               (rest attributes)))))

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
