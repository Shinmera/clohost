(in-package #:org.shirakumo.clohost)

(defclass entity ()
  ((id :initarg :id :accessor id)
   (client :initarg :client :accessor client)))

(defgeneric edit (entity &key))
(defgeneric destroy (entity))

(defgeneric decode-entity (type data &rest initargs))

(defmethod decode-entity ((type symbol) data &rest initargs)
  (apply #'decode-entity (allocate-instance (find-class type)) data initargs))

(defmethod decode-entity ((type symbol) (data list) &rest initargs)
  (loop for entry in data
        collect (apply #'decode-entity type entry initargs)))

(defmacro define-entity (name direct-superclasses &body slots)
  (let ((*print-case* (readtable-case *readtable*))
        (slots (loop for slot in slots collect (if (listp slot) slot (list slot))))
        (data (gensym "DATA"))
        (value (gensym "VALUE")))
    `(progn
       (defclass ,name (,@direct-superclasses entity)
         ,(loop for (slot . options) in slots
                collect `(,slot :initarg ,(intern (string slot) "KEYWORD")
                                :accessor ,slot)))
       (defmethod decode-entity ((,name ,name) ,data &rest initargs)
         (when initargs (apply #'reinitialize-instance ,name initargs))
         ,@(loop for (slot . options) in slots
                 for field = (getf options :field #1='#.(make-symbol "no value"))
                 when field
                 collect `(let ((,value ,(cond ((null field) data)
                                               ((eq field #1#) `(getj ,data ,(to-key slot)))
                                               (T `(getj ,data ,field)))))
                            (setf (slot-value ,name ',slot)
                                  (when ,value
                                    ,(cond ((getf options :from-cache)
                                            `(find-cached (client ,name) ,(getf options :from-cache) ,value))
                                           ((getf options :translate-with)
                                            `(funcall ,(getf options :translate-with) ,value))
                                           (T
                                            value))))))
         ,name)

       (defun ,(intern (format NIL "~a-~a" 'decode name)) (,data)
         (decode-entity ',name ,data)))))

(define-entity account (client)
  (%cache :field NIL)
  (pages :field NIL)
  (notifications :field NIL)
  (default-page :field "projectId")
  mod-mode
  (activated-p :field "activated")
  (read-only-p :field "readOnly"))

(defmethod update-instance-for-different-class :after ((client client) (account account) &key)
  (setf (%cache client) (alexandria:plist-hash-table (list 'page (make-hash-table :test 'equal)
                                                           'post (make-hash-table :test 'equal)
                                                           'comment (make-hash-table :test 'equal))))
  (let ((data (getj (request account :get "/trpc/login.loggedin") :result :data)))
    (decode-entity account data)))

(defmethod cache ((account account) entity)
  (setf (gethash (id entity) (gethash (type-of entity) (%cache account))) entity))

(defmethod find-cached ((account account) type id)
  (gethash id (gethash type (%cache account))))

(defmethod pages :before ((account account))
  (unless (slot-boundp account 'pages)
    (setf (pages account)
          (loop for project in (getj (request account :get "/trpc/projects.listEditedProjects") :result :data :projects)
                collect (decode-entity 'page project :client account)))))

(defmethod notifications :before ((account account))
  (unless (slot-boundp account 'notifications)
    (setf (notifications account)
          (loop for offset from 0 by 100
                for data = (request account :get "/notifications/list" :offset offset :limit 100)
                do (decode-entity 'comment (getj data :comments) :client account)
                   (decode-entity 'post (getj data :posts) :client account)
                   (decode-entity 'page (getj data :projects) :client account)
                nconc (decode-entity 'notification (ungroup-notifications (getj data :projects)) :client account)))))

(defmethod default-page :before ((account account))
  (unless (typep (slot-value account 'default-page) 'page)
    (setf (default-page account) (find-cached account 'page (slot-value account 'default-page)))))

(defmethod edit ((account account) &key default-page)
  (when default-page
    (request account :postjson "/trpc/projects.switchProject" :project-id (id default-page))
    (setf (default-page account) default-page)))

(defclass cached-entity (entity)
  ())

(defmethod decode-entity :after ((entity cached-entity) data &rest args)
  (declare (ignore args))
  (cache (client entity) entity))

(define-entity page (cached-entity)
  (id :key "projectId")
  handle
  (posts :initform () :field NIL)
  (notifications :initform () :field NIL)
  display-name
  (title :key "dek")
  description
  (avatar :key "avatarURL")
  (header :key "headerURL")
  privacy
  url
  pronouns
  flags)

(defgeneric make-post (page &key))
(defgeneric ask (page content &key source))

(defmethod edit ((page page) &key display-name title description avatar header privacy bio pronouns flags info)
  )

(defmethod make-post ((page page) &key title content-warnings adult-p tags content files share)
  )

(defmethod ask ((page page) content &key (source (client page)))
  )

(defmethod posts :before ((page page))
  (unless (slot-boundp account 'posts)
    ))

(define-entity post (cached-entity)
  title
  (time :key "createdAt" :transform-by #'unix->universal)
  state
  content-warnings
  adult-p
  liked-p
  shareable-p
  publishable-p
  related-pages
  tags
  page
  author
  share-tree
  url
  comments
  content
  text)

(defgeneric share (post &key))
(defgeneric reply (post text &key))

(defmethod share ((post post) &key tags content (page (default-page (client post))))
  (apply #'make-post page args :tags tags :content content :share post))

(defmethod reply ((post post) text &key reply-to)
  )

(defmethod edit ((post post) &key title content-warnings adult-p tags content)
  )

(defmethod destroy ((post post))
  )

(defmethod comments :before ((post post))
  (unless (slot-boundp account 'comments)
    ))

(define-entity comment (cached-entity)
  post
  reply-to
  replies
  author
  time
  text)

(defmethod edit ((comment comment) &key text)
  )

(defmethod destroy ((comment comment))
  )

(defmethod reply ((comment comment) text &key)
  (reply (post comment) text :reply-to comment))

(define-entity notification ()
  (time :key "createdAt" :transform-by #'unix->universal)
  (page :key "fromProjectId" :from-cache 'page)
  (post :key "toPostId" :from-cache 'post)
  (share :key "sharePostId" :from-cache 'post)
  (comment :key "commentId" :from-cache 'comment))
