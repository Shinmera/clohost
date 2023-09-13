(in-package #:org.shirakumo.clohost)

(defgeneric edit (entity &key))
(defgeneric destroy (entity))
(defgeneric make-post (page &key))
(defgeneric ask (page content &key source))
(defgeneric share (post &key))
(defgeneric reply (post text &key))

(defclass entity ()
  ((id :initarg :id :accessor id)
   (client :initarg :client :accessor client)))

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
                                               ((listp field) `(getj ,data ,@field))
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

(defmethod notifications ((account account) &key (start 0) (end 10) (per-page 100))
  (loop with offset = start
        while (< offset end)
        for limit = (min per-page (- end offset))
        for data = (request account :get "/notifications/list" :offset offset :limit limit)
        do (decode-entity 'comment (getj data :comments) :client account)
           (decode-entity 'post (getj data :posts) :client account)
           (decode-entity 'page (getj data :projects) :client account)
           (incf offset limit)
        nconc (decode-entity 'notification (ungroup-notifications (getj data :notifications)) :client account)))

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

(define-entity attachment ()
  (id :nullable T :field "attachmentId")
  file
  filename
  content-type
  content-length
  alt-text)

(defmethod initialize-instance :after ((attachment attachment) &key file)
  (unless (slot-boundp attachment 'filename)
    (setf (filename attachment) (file-namestring file)))
  (unless (slot-boundp attachment 'content-type)
    (setf (content-type attachment) (trivial-mimes:mime-type file)))
  (unless (slot-boundp attachment 'content-length)
    (setf (content-length attachment)
          (with-open-file (stream file :element-type '(unsigned-byte 8))
            (file-length file)))))

(define-entity page (cached-entity)
  (id :field "projectId")
  handle
  display-name
  (title :field "dek")
  description
  (avatar :field "avatarURL")
  (header :field "headerURL")
  privacy
  url
  pronouns
  flags)

(defmethod posts ((page page) &key (start 0) (end 25))
  (loop for page from (floor start per-page) below (ceiling end per-page) by per-page
        for data = (request (client page) :get (format NIL "/project/~a/posts" (handle page)) :page page)
        nconc (decode-entity 'post (getj data :items) :client (client page))))

(defun encode-blocks (blocks)
  (loop for block in blocks
        collect (etypecase block
                  (string (tab :type "markdown"
                               :markdown (tab :content block)))
                  (attachment (tab :type "attachment"
                                   :attachment (tab :attachment-id (or (id block) "00000000-0000-0000-0000-000000000000")
                                                    :alt-text (alt-text block)))))))

(defun decode-blocks (blocks)
  (loop for block in blocks
        collect (cond ((string= "markdown" (getj block :type))
                       (getj block :markdown :content))
                      ((string= "attachment" (getj block :type))
                       (decode-entity 'attachment (getj block :attachment)))
                      (T
                       block))))

(defmethod make-post ((page page) &key title content content-warnings tags adult-p draft-p share)
  (let* ((blocks (loop for block in (if (listp content) (list content) content)
                       collect (etypecase thing
                                 (string thing)
                                 (attachment thing)
                                 (pathname (make-instance 'attachment :file thing)))))
         (data (request (client page) :postjson (format NIL "/project/~a/posts" (handle page))
                                      :post-state 0
                                      :headline title
                                      :adult-content adult-p
                                      :blocks (encode-blocks blocks)
                                      :cws content-warnings
                                      :tags tags))
         (post (decode-entity 'post data :client (client page))))
    (loop for thing in blocks
          when (typep content 'attachment)
          do (upload thing post))
    (let ((data (request (client page) :put (format NIL "/project/~a/posts/~a" (handle page) (id post))
                                       :post-state 1
                                       :headline title
                                       :adult-content adult-p
                                       :blocks (encode-blocks blocks)
                                       :cws content-warnings
                                       :tags tags)))
      (decode-entity post data))))

(defmethod ask ((page page) content &key (source (default-page (client page))))
  (request (client page) :postjson "/trpc/asks.send"
           :to-project-handle (handle page)
           :content content
           :anon (not (null source))))

(defmethod edit ((page page) &key display-name title description avatar header privacy bio pronouns flags info)
  )

(define-entity post (cached-entity)
  (id :field "postId")
  (title :field "headline")
  (time :field "publishedAt" :transform-by #'unix->universal)
  state
  (content-warnings :field "cws")
  (adult-p :field "effectiveAdultContent")
  (liked-p :field "isLiked")
  (shareable-p :field "canShare")
  (publishable-p :field "canPublish")
  (related-pages :field "relatedProjects" :from-cache 'page)
  tags
  (page :field "postingProject")
  (op :field ("shareTree" 0 "postingProject"))
  share-tree
  (url :field "singlePostPageUrl")
  (content :field "blocks" :transform-by #'decode-blocks)
  (text :field "plainTextBody")
  comments)

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

(defmethod upload ((attachment attachment) (post post))
  (if (id attachment)
      attachment
      (let ((creds (request (client post) :postjson (format NIL "/project/~a/posts/~a/attach/start" (handle (page post)) (id post))
                                          :filename (filename attachment)
                                          :content_type (content-type attachment)
                                          :content_length (content-length attachment))))
        (drakma:http-request (getj creds :url) :method :post :form-data T :parameters
                             (list* (list "file" (file attachment) :filename (filename attachment) :content-type (content-type attachment))
                                    (alexandria:hash-table-alist (getj creds :required-fields))))
        (request (client post) :post (format NIL "/project/~a/posts/~a/attach/finish/~a" (handle (page post)) (id post) (getj creds :attachment-id)))
        (setf (id attachment) (getj creds :attachment-id))
        attachment)))

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
  (time :field "createdAt" :transform-by #'unix->universal)
  (author :field "fromProjectId" :from-cache 'page)
  (post :field "toPostId" :from-cache 'post)
  (share :field "sharePostId" :from-cache 'post)
  (comment :field "commentId" :from-cache 'comment))

(defun ungroup-notifications (data)
  (let ((notifications ()))
    (dolist (entry data (nreverse notifications))
      (cond ((starts-with "grouped" (getj entry :type))
             (loop for from-project-id in (getj entry :from-project-ids)
                   for relationship-id in (or (getj entry :relationship-ids) (alexandria:make-circular-list 1))
                   for share-post-id in (or (getj entry :share-post-ids) (alexandria:make-circular-list 1))
                   do (push (tab* entry
                                  :type (subseq (getj entry :type) (length "grouped"))
                                  :from-project-id from-project-id
                                  :relationship-id relationship-id
                                  :share-post-id share-post-id)
                            notifications)))
            (T
             (push entry notifications))))))
