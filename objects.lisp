(in-package #:org.shirakumo.clohost)

(defgeneric edit (entity &key))
(defgeneric destroy (entity))
(defgeneric make-post (page &key))
(defgeneric ask (page content &key source))
(defgeneric share (post &key))
(defgeneric reply (post text &key))
(defgeneric notifications (account &key start end per-page))
(defgeneric posts (page &key start end))
(defgeneric ask (page content &key source))
(defgeneric asks (page))
(defgeneric find-post (id page))
(defgeneric comments (post))

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
  (let ((default (default-page (client page))))
    (cond ((not (eq page default)) ;; Wow! This sucks!
           (edit (client page) :default-page page)
           (unwind-protect (notifications (client page) :start start :end end :per-page per-page)
             (edit (client page) :default-page default)))
          (T
           (notifications (client page) :start start :end end :per-page per-page)))))

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

(defun ensure-content-blocks (content)
  (loop for block in (if (listp content) content (list content))
        collect (etypecase block
                  (string block)
                  (attachment block)
                  (pathname (make-instance 'attachment :file block)))))

(defmethod make-post ((page page) &key title content content-warnings tags adult-p draft-p share)
  (let* ((blocks (ensure-content-blocks content))
         (data (request (client page) :postjson (format NIL "/project/~a/posts" (handle page))
                                      :post-state 0
                                      :headline title
                                      :adult-content adult-p
                                      :blocks (encode-blocks blocks)
                                      :cws content-warnings
                                      :tags tags
                                      :share-of-post-id (when share (id share))))
         (post (decode-entity 'post data :client (client page))))
    (loop for thing in blocks
          when (typep thing 'attachment)
          do (upload thing post))
    (let ((data (request (client page) :put (format NIL "/project/~a/posts/~a" (handle page) (id post))
                                       :post-state (if draft-p 1 0)
                                       :headline title
                                       :adult-content adult-p
                                       :blocks (encode-blocks blocks)
                                       :cws content-warnings
                                       :tags tags
                                       :share-of-post-id (when share (id share)))))
      (decode-entity post data))))

(defmethod edit ((page page) &key display-name title description url pronouns contact-card)
  ;; FIXME: avatar and header changes. Somehow the chrome network tab shows no upload for it...?
  (request (client page) :post "/versions"
                         :display-name (or display-name (display-name page))
                         :dek (or title (title page))
                         :description (or description (description page))
                         :url (or url (url page))
                         :pronouns (or pronouns (pronouns page))
                         :contact-card (com.inuoe.jzon:stringify (or contact-card (contact-card page))))
  (when display-name (setf (display-name page) display-name))
  (when title (setf (title page) title))
  (when description (setf (description page) description))
  (when url (setf (url page) url))
  (when pronouns (setf (pronouns page) pronouns))
  (when contact-card (setf (contact-card page) contact-card))
  page)

(defmethod ask ((page page) content &key (source (default-page (client page))))
  (request (client page) :postjson "/trpc/asks.send"
                         :to-project-handle (handle page)
                         :content content
                         :anon (not (null source))))

(defmethod asks ((page page))
  ;; FIXME: implement
  (error "Not implemented"))

(defmethod find-post ((id integer) (page page))
  (let ((response (request (client page) :get "/trpc/posts.singlePost"
                                         :batch 1
                                         :input (tab "0" id "1" (tab :handle (handle page) :post-id id)))))
    (let ((data (getj response :result :data :post)))
      (setf (gethash "comments" data) (getj response :result :comments (getj data :post-id)))
      (decode-entity 'post data))))

(defmethod find-post ((id string) (page page))
  (find-post (parse-integer id) page))

(define-entity post (cached-entity)
  (id :field "postId")
  (title :field "headline")
  (time :field "publishedAt" :translate-with #'to-universal)
  state
  (content-warnings :field "cws" :translate-with #'to-list)
  (adult-p :field "effectiveAdultContent")
  (liked-p :field "isLiked")
  (shareable-p :field "canShare" :translate-with #'from-bool)
  (publishable-p :field "canPublish" :translate-with #'from-bool)
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
  (comments :to-cache 'comment))

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
  (if reply-to
      (request (client post) :postjson "/comments"
                             :postid (id post)
                             :body text
                             :in-reply-to-comment-id (id reply-to))
      (request (client post) :postjson "/comments"
                             :postid (id post)
                             :body text)))

(defmethod edit ((post post) &key title content-warnings (adult-p NIL adult-pp) (tags NIL tags-p) draft-p content (liked-p NIL liked-pp))
  (when liked-pp
    (request (client post) :postjson (if liked-p "/trpc/relationships.like" "/trpc/relationships.unlike")
                           :from-project-id (id (client post)) 
                           :to-post-id (id post))
    (setf (liked-p post) liked-p))
  (when (or title content-warnings adult-p tags content)
    (let ((blocks (if content (ensure-content-blocks content) (content post))))
      (loop for thing in blocks
            when (typep thing 'attachment)
            do (upload thing post))
      (let ((data (request (client post) :put (format NIL "/project/~a/posts/~a" (handle (page post)) (id post))
                                         :post-state (if draft-p 1 0)
                                         :headline (or title (title post))
                                         :adult-content (if adult-pp adult-p (adult-p post))
                                         :blocks (encode-blocks blocks)
                                         :cws (or content-warnings (content-warnings post))
                                         :tags (if tags-p tags (tags post)))))
        (decode-entity post data)))))

(defmethod destroy ((post post))
  (request (client post) :postjson "/trpc/posts.delete"
                         :project-handle (handle (page post)) 
                         :post-id (id post))
  (slot-makunbound post 'id)
  post)

(defmethod comments :before ((post post))
  (unless (slot-boundp post 'comments)
    ;; This will fetch the post through the single endpoint and refresh comments.
    (find-post (id post) (page post))))

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
  (reply-to :field NIL)
  (post :field ("comment" "postId"))
  (replies :field ("comment" "children") :to-cache 'comment)
  (time :field ("comment" "postedAtISO") :translate-with #'to-universal)
  (text :field ("comment" "body"))
  (deleted-p :field ("comment" "deleted"))
  (hidden-p :field ("comment" "hidden"))
  (author :field "poster" :to-cache 'post)
  (interactable-p :field "canInteract" :translate-with #'from-bool)
  (editable-p :field "canEdit" :translate-with #'from-bool)
  (hideable-p :field "canHide" :translate-with #'from-bool))

(defmethod print-object ((entity comment) stream)
  (if (slot-boundp entity 'id)
      (print-unreadable-object (entity stream :type T)
        (format stream "~a @~a" (handle (author entity)) (id entity)))
      (call-next-method)))

(defmethod decode-entity :after ((comment comment) data &rest args)
  (declare (ignore args))
  (loop for child in (replies comment)
        do (setf (reply-to child) comment)))

(defmethod edit ((comment comment) &key text (hidden-p NIL hidden-pp))
  (when text
    (request (client comment) :put (format NIL "/comments/~a" (id comment))
                              :post-id (id (post comment))
                              :body text)
    (setf (text comment) text))
  (when hidden-pp
    (request (client comment) :postjson (format NIL "/trpc/comments.setHidden")
                              :comment-id (id comment)
                              :hidden hidden-p)
    (setf (hidden-p comment) hidden-p))
  comment)

(defmethod destroy ((comment comment))
  (request (client comment) :delete (format NIL "/comments/~a" (id comment))
                            :post-id (id (post comment))
                            :body "")
  (slot-makunbound comment 'id)
  comment)

(defmethod reply ((comment comment) text &key)
  (reply (post comment) text :reply-to comment))

(define-entity notification ()
  (kind :field "type" :translate-with #'to-keyword)
  (time :field "createdAt" :translate-with #'to-universal)
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
