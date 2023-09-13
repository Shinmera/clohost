(in-package #:org.shirakumo.clohost)

(define-condition clohost-error (error)
  ((endpoint :initarg :endpoint :reader endpoint)
   (parameters :initarg :parameters :initform () :reader parameters)
   (response :initarg :response :initform () :reader response))
  (:report (lambda (c s) (format s "The request to~%  ~a~@[ ~s~]~%failed~@[ with result:~%~a~]"
                                 (endpoint c) (parameters c) (com.inuoe.jzon:stringify (response c) :pretty T)))))

(defclass client ()
  ((cookie-jar :initform (make-instance 'drakma:cookie-jar) :accessor cookie-jar)))

(defmethod shared-initialize :after ((client client) slots &key (token NIL token-p))
  (when token-p
    (setf (drakma:cookie-jar-cookies (cookie-jar client))
          (if token-p (list (make-instance 'drakma:cookie :domain "cohost.org" :name "connect.sid" :value token)) ()))))

(defmethod request ((client client) method endpoint &rest args)
  (multiple-value-bind (stream status headers)
      (drakma:http-request (format NIL "https://cohost.org/api/v1~a" endpoint)
                           :method (ecase method
                                     ((:get :post :put) method)
                                     (:postjson :post))
                           :parameters (case method
                                         ((:get :post)
                                          (loop for (k v) on args by #'cddr
                                                collect (cons (to-key k)
                                                              (etypecase v
                                                                (string v)
                                                                ((eql T) "true")
                                                                ((eql NIL) "false")
                                                                (integer (princ-to-string v)))))))
                           :content (case method
                                      ((:postjson :put)
                                       (com.inuoe.jzon:stringify
                                        (loop with table = (make-hash-table :test 'equal)
                                              for (k v) on args by #'cddr
                                              do (setf (gethash (to-key k) table) v)
                                              finally (return table)))))
                           :content-type (ecase method
                                           (:get "application/x-www-form-urlencoded")
                                           (:post "multipart/form-data")
                                           ((:postjson :put) "application/json"))
                           :cookie-jar (cookie-jar client)
                           :user-agent "Clohost"
                           :external-format-in :utf-8
                           :external-format-out :utf-8
                           :want-stream T)
    (let ((payload (com.inuoe.jzon:parse stream)))
      (format *error-output* "~a" (com.inuoe.jzon:stringify payload :pretty T))
      (when (<= 400 status)
        (error 'clohost-error :endpoint endpoint :parameters args :response payload))
      (values payload headers))))

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
    (change-class client 'account :id (gethash "userId" result))))

(defmethod logout ((client client))
  (request client :post "/logout")
  (change-class client 'client :token NIL))

(defmethod token ((client client))
  (drakma:cookie-value (find "connect.sid" (drakma:cookie-jar-cookies (cookie-jar client)) :key #'drakma:cookie-name :test #'string=)))
