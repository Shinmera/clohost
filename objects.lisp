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

(defmethod print-object ((entity entity) stream)
  (if (slot-boundp entity 'id)
      (print-unreadable-object (entity stream :type T)
        (format stream "@~a" (id entity)))
      (call-next-method)))

(defgeneric decode-entity (type data &rest initargs))

(defmethod decode-entity ((type symbol) data &rest initargs)
  (apply #'decode-entity (allocate-instance (find-class type)) data initargs))

(defmethod decode-entity ((type symbol) (data list) &rest initargs)
  (loop for entry in data
        collect (apply #'decode-entity type entry initargs)))

(defmethod decode-entity ((type symbol) (data vector) &rest initargs)
  (loop for entry across data
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
                                           ((getf options :to-cache)
                                            `(decode-entity ,(getf options :to-cache) ,value :client (client ,name)))
                                           ((getf options :translate-with)
                                            `(funcall ,(getf options :translate-with) ,value))
                                           (T
                                            value))))))
         ,name)

       (defmethod update-with ((,data ,name) (,name ,name))
         (dolist (,value ',(mapcar #'first slots) ,name)
           (when (slot-boundp ,data ,value)
             (setf (slot-value ,name ,value) (slot-value ,data ,value))))))))

(define-entity account (client)
  (%cache :field NIL)
  (pages :field NIL)
  email
  (default-page :field "projectId")
  mod-mode
  (activated-p :field "activated")
  (read-only-p :field "readOnly")
  (email-verified-p :field "emailVerified")
  (two-factor-p :field "twoFactorActive"))

(defmethod shared-initialize :after ((account account) slots &key)
  (unless (slot-boundp account '%cache)
    (setf (%cache account) (alexandria:plist-hash-table (list 'page (tab) 'post (tab) 'comment (tab))))))

(defmethod update-instance-for-different-class :after ((client client) (account account) &key)
  (setf (client account) account)
  (decode-entity account (getj (request account :get "/trpc/login.loggedIn") :result :data)))

(defmethod print-object ((entity account) stream)
  (if (slot-boundp entity 'id)
      (print-unreadable-object (entity stream :type T)
        (format stream "~a @~a" (email entity) (id entity)))
      (call-next-method)))

(defmethod cache ((account account) entity)
  (setf (gethash (id entity) (gethash (type-of entity) (%cache account))) entity))

(defmethod find-cached ((account account) type id)
  (or (gethash id (gethash type (%cache account)))
      id))

(defmethod clear-cache ((account account))
  (loop for table being the hash-values of (%cache account)
        do (clrhash table)))

(defmethod pages :before ((account account))
  (unless (slot-boundp account 'pages)
    (setf (pages account)
          (loop for project across (getj (request account :get "/trpc/projects.listEditedProjects") :result :data :projects)
                collect (decode-entity 'page project :client account)))))

(defmethod notifications ((account account) &key (start 0) (end 10) (per-page 100))
  (loop with offset = start
        while (< offset end)
        for limit = (min per-page (- end offset))
        for data = (request account :get "/notifications/list" :offset offset :limit limit)
        do (decode-entity 'comment (alexandria:hash-table-values (getj data :comments)) :client account)
           (decode-entity 'post (alexandria:hash-table-values (getj data :posts)) :client account)
           (decode-entity 'page (alexandria:hash-table-values (getj data :projects)) :client account)
           (incf offset limit)
        nconc (decode-entity 'notification (ungroup-notifications (getj data :notifications)) :client account)))

(defmethod default-page :before ((account account))
  (unless (typep (slot-value account 'default-page) 'page)
    (setf (default-page account) (find-cached account 'page (slot-value account 'default-page)))))

(defmethod edit ((account account) &key default-page)
  (etypecase default-page
    (page
     (request account :postjson "/trpc/projects.switchProject" :project-id (id default-page))
     (setf (default-page account) default-page))
    (string
     (edit account :default-page (find default-page (pages account) :key #'handle :test #'string=)))
    (null)))

(defclass cached-entity (entity)
  ())

(defmethod decode-entity :around ((entity cached-entity) data &rest args)
  (declare (ignore args))
  (let ((result (call-next-method)))
    (cond ((not (slot-boundp result 'id))
           result)
          ((find-cached (client entity) (type-of result) (id result))
           (let ((existing (find-cached (client entity) (type-of result) (id result))))
             (update-with result existing)))
          (T
           (cache (client entity) entity)))))

(define-entity attachment ()
  (id :field "attachmentId")
  (kind :translate-with #'to-keyword)
  (url :field "fileURL")
  (preview :field "previewURL")
  width
  height
  file
  filename
  content-type
  content-length
  alt-text)

(defmethod initialize-instance :after ((attachment attachment) &key file)
  (unless (slot-boundp attachment 'filename)
    (setf (filename attachment) (file-namestring file)))
  (unless (slot-boundp attachment 'content-type)
    (setf (content-type attachment) (trivial-mimes:mime file)))
  (unless (slot-boundp attachment 'content-length)
    (setf (content-length attachment)
          (with-open-file (stream file :element-type '(unsigned-byte 8))
            (file-length file)))))

(defmethod print-object ((entity attachment) stream)
  (print-unreadable-object (entity stream :type T)
    (format stream "~a~@[ @~a~]" (filename entity) (id entity))))

(define-entity page (cached-entity)
  (id :field "projectId")
  handle
  display-name
  (title :field "dek")
  description
  (avatar :field "avatarURL")
  (header :field "headerURL")
  (privacy :translate-with #'to-keyword)
  url
  pronouns
  (flags :translate-with #'to-list)
  (ask-settings :translate-with #'from-tab)
  (contact-card :translate-with #'from-tab))

(defmethod print-object ((entity page) stream)
  (if (slot-boundp entity 'id)
      (print-unreadable-object (entity stream :type T)
        (format stream "~a @~a" (handle entity) (id entity)))
      (call-next-method)))

(defmethod posts ((page page) &key (start 0) (end 25))
  (loop with per-page = 25
        for index from (floor start per-page) below (ceiling end per-page)
        for data = (request (client page) :get (format NIL "/project/~a/posts" (handle page)) :page index)
        nconc (decode-entity 'post (getj data :items) :client (client page))))

(defmethod notifications ((page page) &key (start 0) (end 10) (per-page 100))
  ;; TODO: implement
  (error "Not implemented"))

(defun encode-blocks (blocks)
  (loop for block in blocks
        collect (etypecase block
                  (string (tab :type "markdown"
                               :markdown (tab :content block)))
                  (attachment (tab :type "attachment"
                                   :attachment (tab :attachment-id (or (id block) "00000000-0000-0000-0000-000000000000")
                                                    :alt-text (alt-text block)))))))

(defun decode-blocks (blocks)
  (loop for block across blocks
        collect (cond ((string= "markdown" (getj block :type))
                       (getj block :markdown :content))
                      ((string= "attachment" (getj block :type))
                       (decode-entity 'attachment (getj block :attachment)))
                      (T
                       block))))

(defmethod make-post ((page page) &key title content content-warnings tags adult-p draft-p share)
  (let* ((blocks (loop for block in (if (listp content) (list content) content)
                       collect (etypecase block
                                 (string block)
                                 (attachment block)
                                 (pathname (make-instance 'attachment :file block)))))
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
  ;; TODO: implement
  (error "Not implemented"))

(define-entity post (cached-entity)
  (id :field "postId")
  (title :field "headline")
  (time :field "publishedAt" :translate-with #'to-universal)
  state
  (content-warnings :field "cws" :translate-with #'to-list)
  (adult-p :field "effectiveAdultContent")
  (liked-p :field "isLiked")
  (shareable-p :field "canShare")
  (publishable-p :field "canPublish")
  (pinned-p :field "pinned")
  (comments-locked-p :field "commentsLocked")
  (shares-locked-p :field "sharesLocked")
  (related-pages :field "relatedProjects" :from-cache 'page)
  (tags :translate-with #'to-list)
  (page :field "postingProject" :to-cache 'page)
  (share-tree :to-cache 'post)
  (url :field "singlePostPageUrl")
  (content :field "blocks" :translate-with #'decode-blocks)
  (text :field "plainTextBody")
  (comments :field NIL))

(defmethod print-object ((entity post) stream)
  (if (slot-boundp entity 'id)
      (print-unreadable-object (entity stream :type T)
        (format stream "~@[~s ~]@~a" (title entity) (id entity)))
      (call-next-method)))

(defmethod op ((post post))
  (or (first (share-tree post))
      (page post)))

(defmethod share ((post post) &key tags content (page (default-page (client post))))
  (make-post page :tags tags :content content :share post))

(defmethod reply ((post post) text &key reply-to)
  ;; TODO: implement
  (error "Not implemented"))

(defmethod edit ((post post) &key title content-warnings adult-p tags content)
  ;; TODO: implement
  (error "Not implemented"))

(defmethod destroy ((post post))
  ;; TODO: implement
  (error "Not implemented"))

(defmethod comments :before ((post post))
  (unless (slot-boundp post 'comments)
    ;; TODO: implement
    (error "Not implemented")))

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

(defmethod print-object ((entity comment) stream)
  (if (slot-boundp entity 'id)
      (print-unreadable-object (entity stream :type T)
        (format stream "~a @~a" (handle (author entity)) (id entity)))
      (call-next-method)))

(defmethod edit ((comment comment) &key text)
  ;; TODO: implement
  (error "Not implemented"))

(defmethod destroy ((comment comment))
  ;; TODO: implement
  (error "Not implemented"))

(defmethod reply ((comment comment) text &key)
  (reply (post comment) text :reply-to comment))

(define-entity notification ()
  (kind :field "type" :translate-with #'to-keyword)
  (time :field "createdAt" :translate-with #'to->universal)
  (author :field "fromProjectId" :from-cache 'page)
  (post :field "toPostId" :from-cache 'post)
  (share-post :field "sharePostId" :from-cache 'post)
  (comment :field "commentId" :from-cache 'comment))

(defmethod print-object ((entity notification) stream)
  (print-unreadable-object (entity stream :type T)
    (format stream "~a ~a ~a" (kind entity) (handle (author entity)) (universal->utc-timestring (time entity)))))

(defun ungroup-notifications (data)
  (let ((notifications ()))
    (loop for entry across data
          do (cond ((starts-with "grouped" (getj entry :type))
                    (loop for i from 0
                          for from-project-id across (getj entry :from-project-ids)
                          for relationship-id = (getj entry :relationship-ids i)
                          for share-post-id =  (getj entry :share-post-ids i) 
                          do (push (tab* entry
                                         :type (subseq (getj entry :type) (length "grouped"))
                                         :from-project-id from-project-id
                                         :relationship-id relationship-id
                                         :share-post-id share-post-id)
                                   notifications)))
                   (T
                    (push entry notifications)))
          finally (return (nreverse notifications)))))
