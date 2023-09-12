(in-package #:org.shirakumo.clohost)

(define-condition clohost-error (error)
  ((endpoint :initarg :endpoint :reader endpoint)
   (parameters :initarg :parameters :initform () :reader parameters)
   (response :initarg :response :initform () :reader response))
  (:report (lambda (c s) (format s "The request to~%  ~a~@[ ~a~]~%failed~@[ with result:~%  ~a~]"
                                 (endpoint c) (parameters c) (response c)))))

(defclass client ()
  ((token :initarg :token :initform NIL :accessor token)
   (cookie-jar :initform (make-instance 'drakma:cookie-jar) :accessor cookie-jar)))

(defmethod shared-initialize :after ((client client) slots &key (token NIL token-p))
  (setf (drakma:cookie-jar-cookies (cookie-jar client))
        (if token-p (list (make-instance 'drakma:cookie :domain "cohost.org" :name "connect.sid" :value token)) ())))

(defmethod request ((client client) method endpoint &rest args)
  (multiple-value-bind (stream status headers)
      (drakma:http-request (format NIL "https://cohost.org/api/v1~a" endpoint)
                           :method (ecase method
                                     ((:get :post :put) method)
                                     (:postjson :post))
                           :parameters (case method
                                         ((:get :post)
                                          (loop for (k v) on args by #'cddr
                                                collect (cons (to-key k) v))))
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
      (when (<= 400 status)
        (error 'clohost-error :endpoint endpoint :parameters args :response payload))
      (values payload headers))))

(defun fixup-salt (salt)
  (cryptos:from-base64 (format NIL "~a==" (nsubstitute #\_ #\A (nsubstitute #\A #\- salt)))))

(defmethod login ((client client) email password)
  (let* ((salt (fixup-salt (request client :get "/login/salt")))
         (hash (cryptos:pbkdf2-hash password salt :iterations 200000 :digest :sha128 :to :base64))
         (result (request client :post "/login" :email email :client-hash hash))
         (cookie (find "connect.sid" (drakma:cookie-jar-cookies (cookie-jar client)) :key #'drakma:cookie-name :test #'string=)))
    (setf (slot-value client 'token) (drakma:cookie-value cookie))
    (change-class client 'account :id (gethash "userId" result))))

(defmethod logout ((client client))
  (request client :post "/logout")
  (change-class client 'client :token NIL))
