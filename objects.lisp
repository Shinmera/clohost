(in-package #:org.shirakumo.clohost)

(defclass entity ()
  ((id :initarg :id :accessor id)
   (client :initarg :client :accessor client)))

(defgeneric edit (entity &key))
(defgeneric destroy (entity))

(defgeneric decode-entity (type data))

(defmethod decode-entity ((type symbol) data)
  (decode-entity (allocate-instance (find-class type)) data))

(defmethod decode-entity ((type symbol) (data list))
  (loop for entry in data
        collect (decode-entity type entry)))

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
       (defmethod decode-entity ((,name ,name) ,data)
         ,@(loop for (slot . options) in slots
                 for field = (getf options :field #1='#.(make-symbol "no value"))
                 collect `(let ((,value ,(cond ((null field) data)
                                               ((eq field #1#) `(getj ,data ,(to-key slot)))
                                               (T `(getj ,data ,field)))))
                            (setf (slot-value ,name ',slot)
                                  (when ,value
                                    (funcall ,(or (getf options :translate-with)
                                                  '#'identity)
                                             ,value)))))
         ,name)

       (defun ,(intern (format NIL "~a-~a" 'decode name)) (,data)
         (decode-entity ',name ,data)))))

(define-entity account (client)
  pages
  default-page
  mod-mode
  activated-p
  read-only-p)

(defmethod pages :before ((account account))
  (unless (slot-boundp account 'pages)
    ))

(defmethod edit ((account account) &key default-page)
  )

(define-entity page ()
  posts
  notifications
  display-name
  title
  description
  avatar
  header
  privacy
  bio
  pronouns
  flags
  info)

(defgeneric make-post (page &key))

(defmethod edit ((page page) &key display-name title description avatar header privacy bio pronouns flags info)
  )

(defmethod make-post ((page page) &key title content-warnings adult-p tags content files share)
  )

(defmethod posts :before ((page page))
  (unless (slot-boundp account 'posts)
    ))

(define-entity post ()
  title
  time
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

(define-entity comment ()
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
