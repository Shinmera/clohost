(in-package #:org.shirakumo.clohost)

(defvar *debug* NIL)

(define-condition clohost-error (error)
  ((endpoint :initarg :endpoint :reader endpoint)
   (parameters :initarg :parameters :initform () :reader parameters)
   (response :initarg :response :initform () :reader response))
  (:report (lambda (c s) (format s "The request to~%  ~a~@[ ~s~]~%failed~@[ with result:~%~a~]"
                                 (endpoint c) (parameters c) (com.inuoe.jzon:stringify (response c) :pretty T)))))

(defclass client ()
  ((cookie-jar :initform (make-instance 'drakma:cookie-jar) :accessor cookie-jar)))

(defmethod shared-initialize :after ((client client) slots &key (token NIL token-p))
  (when token-p (setf (token client) token)))

(defmethod request ((client client) method endpoint &rest args)
  (when *debug*
    (format *error-output* "~&>> ~a ~a~%" method endpoint))
  (multiple-value-bind (stream status headers)
      (drakma:http-request (format NIL "https://cohost.org/api/v1~a" endpoint)
                           :method (ecase method
                                     ((:get :post :put :delete) method)
                                     (:postjson :post))
                           :parameters (case method
                                         ((:get :post)
                                          (loop for (k v) on args by #'cddr
                                                unless (eql v :||)
                                                collect (cons (to-key k)
                                                              (etypecase v
                                                                (string v)
                                                                ((eql T) "true")
                                                                ((eql NIL) "false")
                                                                (hash-table (com.inuoe.jzon:stringify v))
                                                                (integer (princ-to-string v)))))))
                           :content (case method
                                      ((:postjson :put :delete)
                                       (com.inuoe.jzon:stringify
                                        (loop with table = (make-hash-table :test 'equal)
                                              for (k v) on args by #'cddr
                                              unless (eql v :||)
                                              do (setf (gethash (to-key k) table) v)
                                              finally (return table)))))
                           :content-type (ecase method
                                           (:get "application/x-www-form-urlencoded")
                                           (:post "multipart/form-data")
                                           ((:postjson :put :delete) "application/json"))
                           :cookie-jar (cookie-jar client)
                           :user-agent "Clohost"
                           :external-format-in :utf-8
                           :external-format-out :utf-8
                           :want-stream T)
    (unwind-protect
         (let ((payload (com.inuoe.jzon:parse stream)))
           (when *debug*
             (format *error-output* "~&<< ~a~%" (com.inuoe.jzon:stringify payload :pretty T)))
           (when (<= 400 status)
             (error 'clohost-error :endpoint endpoint :parameters args :response payload))
           (values payload headers))
      (close stream))))

(defun fixup-salt (salt)
  (cryptos:from-base64 (format NIL "~a==" (nsubstitute #\_ #\A (nsubstitute #\A #\- salt))) :octets))

(defun compute-hash (password salt)
  (let* ((password (cryptos:to-octets password))
         (hash (ironclad::pbkdf2-derive-key :sha384 password salt 200000 128)))
    (cryptos:to-base64 hash)))

(defmethod login ((new (eql T)) email password)
  (login (make-instance 'client) email password))

(defmethod login ((client client) email password)
  (let* ((salt (fixup-salt (getj (request client :get "/login/salt" :email email) :salt)))
         (result (request client :post "/login" :email email :client-hash (compute-hash password salt))))
    (setf (token client) (token client))
    client))

(defmethod logout ((client client))
  (request client :post "/logout")
  (setf (token client) NIL))

(defmethod token ((client client))
  (drakma:cookie-value (find "connect.sid" (drakma:cookie-jar-cookies (cookie-jar client)) :key #'drakma:cookie-name :test #'string=)))

(defmethod (setf token) (value (client client))
  (let ((cookie (find "connect.sid" (drakma:cookie-jar-cookies (cookie-jar client)) :key #'drakma:cookie-name :test #'string=)))
    (cond ((null value)
           (setf (drakma:cookie-jar-cookies (cookie-jar client)) ()))
          (cookie
           (setf (drakma:cookie-value cookie) value))
          (T
           (push (make-instance 'drakma:cookie :name "connect.sid" :value value :domain "cohost.org")
                 (drakma:cookie-jar-cookies (cookie-jar client)))))
    (if value
        (unless (typep client 'account)
          (change-class client 'account))
        (when (typep client 'account)
          (change-class client 'client)))
    value))
