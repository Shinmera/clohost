(in-package #:org.shirakumo.clohost)

;; client.lisp
(docs:define-docs
  (type clohost-error
    "Error signalled when an API request fails.

See ENDPOINT
See PARAMETERS
See RESPONSE")
  
  (function endpoint
    "The endpoint that the request was made to.

See CLOHOST-ERROR")
  
  (function parameters
    "The parameters that were supplied to the request.

See CLOHOST-ERROR")
  
  (function response
    "The response body from the failed request.

See CLOHOST-ERROR")
  
  (type client
    "Representation of an API client for Cohost.

A logged in client will be an instance of ACCOUNT.

See REQUEST
See TOKEN
See LOGIN
See LOGOUT
See ACCOUNT")
  
  (function token
    "Accesses the login token used to connect.

See CLIENT (type)")
  
  (function request
    "Perform an API request to Cohost.

METHOD can be one of:
  :GET
  :POST
  :PUT
  :DELETE
  :POSTJSON

Endpoint should be a path relative to the Cohost v1 API.

Signals a condition of type CLOHOST-ERROR if the API request fails.

Returns two values: the parsed payload and reply header alist.

See CLOHOST-ERROR
See CLIENT (type)")
  
  (function login
    "Logs onto Cohost.

If successful, this will change the CLIENT into an ACCOUNT and return
it.

See CLIENT (type)
See ACCOUNT (type)")
  
  (function logout
    "Logs out of Cohost.

If successful, this will change the ACCOUNT back into a CLIENT and
return it.

See CLIENT (type)
see ACCOUNT (type)"))

;; objects.lisp
(docs:define-docs
  (function edit
    "Edit properties of the given object.

The editable properties should be supplied via keyword arguments.

See ENTITY (type)")

  (function destroy
    "Delete the given object.

See ENTITY (type)")

  (function make-post
    "Create a new post on the given page.

The new post instance is returned.

CONTENT should either be a content block or a list of content
blocks. A content block can either be a STRING of markdown formatted
text, a PATHNAME to a file to upload, or an ATTACHMENT to upload.

SHARE, if provided, should be another post which this post will be a
share of.

If DRAFT-P is true, the post will be left in draft status and won't be
published. You'll have to EDIT the post later to publish it.

See ATTACHMENT (type)
See PAGE (type)")

  (function ask
    "Submit a new ask on another page.

See PAGE (type)")

  (function share
    "Share a post on another page.

This is the same as supplying :SHARE to MAKE-POST.

See PAGE (type)
See MAKE-POST")

  (function reply
    "Reply to a post or comment.

See POST (type)
See COMMENT (type)")

  (function notifications
    "List notifications for a page.

See ACCOUNT (type)
See PAGE (type)
See NOTIFICATION (type)")

  (function posts
    "List posts from a page.

See PAGE (type)
See POST (type)")

  (function asks
    "List asks for a page.

See PAGE (type)")

  (function find-post
    "Find a particular post on a page.

See POST (type)
See PAGE (type)")

  (function comments
    "List the comments on a post.

See POST (type)
See COMMENT (type)")

  (type entity
    "Base object representing a foreign object.

See ID
See CLIENT
See EDIT
See DESTROY")

  (function id
    "Returns the ID of the object on the remote API.

See ENTITY (type)")

  (function client
    "Returns the client with which the object was retrieved.

See ENTITY (type)")

  (type account
    "Representation of a user account.

A user account can own multiple pages and perform requests on behalf
of them.

See CLIENT (type)
See ENTITY (type)
See PAGES
See EMAIL
See DEFAULT-PAGE
See MOD-MODE
See READ-ONLY-P
See EMAIL-VERIFIED-P
See TWO-FACTOR-P
See NOTIFICATIONS")

  (function pages
    "Returns all the pages this account can edit.

see ACCOUNT (type)
See PAGE (type)")

  (function email
    "Returns the email address of the account.

See ACCOUNT (type)")

  (function default-page
    "Returns the default page of the account.

See PAGE (type)
See ACCOUNT (type)")

  (function mod-mode
    "?

See ACCOUNT (type)")

  (function activated-p
    "Returns whether the account is activated.

See ACCOUNT (type)")

  (function read-only-p
    "Returns whether the account is in read-only mode.

See ACCOUNT (type)")

  (function email-verified-p
    "Returns whether the account's email has been verified.

See ACCOUNT (type)")

  (function two-factor-p
    "Returns whether two-factor authentication is active for the account.

See ACCOUNT (type)")

  (type attachment
    "Representation of an attachment in a post.

See MAKE-POST
See ENTITY (type)
See KIND
See URL
See PREVIEW
See WIDTH
See HEIGHT
See FILE
See FILENAME
See CONTENT-TYPE
See CONTENT-LENGTH
See ALT-TEXT")

  (function kind
    "Returns the type of entity this represents.

See NOTIFICATION (type)
See ATTACHMENT (type)")

  (function url
    "Returns the URL at which this entity is accessible.

See POST (type)
See PAGE (type)
See ATTACHMENT (type)")

  (function preview
    "Returns the URL at which a minified URL of this entity is accessible.

See ATTACHMENT (type)")

  (function width
    "Returns the width of the attachment.

See ATTACHMENT (type)")

  (function height
    "Returns the height of the attachment.

See ATTACHMENT (type)")

  (function file
    "Accesses the file pathname of the attachment.

See ATTACHMENT (type)")

  (function filename
    "Accesses the filename of the attachment.

See ATTACHMENT (type)")

  (function content-type
    "Accesses the content-type of the attachment.

This is a mime-type string.

See ATTACHMENT (type)")

  (function content-length
    "Accesses the length in octets of the attachment.

See ATTACHMENT (type)")

  (function alt-text
    "Accesses the alt text of the attachment.

See ATTACHMENT (type)")

  (type page
    "Representation of a page on Cohost.

This is called a \"project\" internally by the API. It is effectively
a user in how the site operates, but in order to avoid confusion with
an account, which can own many pages, we call it a \"page\", just like
the UI refers to them.

See ENTITY (type)
See HANDLE
See DISPLAY-NAME
See TITLE
See DESCRIPTION
See AVATAR
See HEADER
See PRIVACY
See URL
See PRONOUNS
See FLAGS
See ASK-SETTINGS
See CONTACT-CARD
See POSTS
See NOTIFICATIONS
See MAKE-POST
See ASK
See ASKS
See FIND-POST")

  (function handle
    "Returns the unique handle of the page.

See PAGE (type)")

  (function display-name
    "Returns the name that should be used for the page.

See PAGE (type)")

  (function title
    "Returns the title of the entity.

For a page, this is its tagline, internally called \"dek\".

See POST (type)
See PAGE (type)")

  (function description
    "Returns the description of the page.

This is markdown formatted text.

See PAGE (type)")

  (function avatar
    "Returns the URL to the page's avatar.

See PAGE (type)")

  (function header
    "Returns the URL to the page's header banner.

See PAGE (type)")

  (function privacy
    "Returns the privacy setting of the page.

See PAGE (type)")

  (function pronouns
    "Returns a string description of the page's preferred pronouns.

See PAGE (type)")

  (function flags
    "Returns a list of flags set on the page.

See PAGE (type)")

  (function ask-settings
    "Returns a plist describing the ask settings.

See PAGE (type)")

  (function contact-card
    "Returns a list of contact cards.

See PAGE (type)")

  (type post
    "Representation of a post on a page.

See ENTITY (type)
See TITLE
See TIME
See STATE
See CONTENT-WARNINGS
See ADULT-P
See LIKED-P
See SHAREABLE-P
See PINNED-P
See COMMENTS-LOCKED-P
See SHARES-LOCKED-P
See RELATED-PAGES
See TAGS
See PAGE
See SHARE-TREE
See URL
See CONTENT
See TEXT
See COMMENTS
See OP
See SHARE
See REPLY")

  (function time
    "Returns the universal-time timstamp of when the entity was made.

See COMMENT (type)
See NOTIFICATION (type)
See POST (type)")

  (function state
    "Returns the state of the post.

See POST (type)")

  (function content-warnings
    "Returns a list of content warnings for the post.

See POST (type)")

  (function adult-p
    "Returns whether the post is marked as adult content.

See POST (type)")

  (function liked-p
    "Returns whether the post was liked from the current default page.

See POST (type)")

  (function shareable-p
    "Returns whether the post can be shared on the current page.

See POST (type)")

  (function publishable-p
    "Returns whether the post can still be published.

See POST (type)")

  (function pinned-p
    "Returns whether the post is pinned to the top of the page.

See POST (type)")

  (function comments-locked-p
    "Returns whether the comments are locked on the post.

See POST (type)")

  (function shares-locked-p
    "Returns whether the sharing the post is restricted.

See POST (type)")

  (function related-pages
    "Returns a list of related pages mentioned in the post.

See POST (type)")

  (function tags
    "Returns a list of tags associated with the post.

See POST (type)")

  (function page
    "Returns the page the entity is associated with.

See PAGE (type)
See POST (type)
See NOTIFICATION (type)")

  (function share-tree
    "Returns the list of posts that encapsulate the shares of the original post.

See POST (type)")

  (function content
    "Returns the post content as a list of blocks.

Each block can either be a string of text in markdown format, or an
ATTACHMENT instance.

See POST (type)
See ATTACHMENT (type)")

  (function text
    "Returns the plaintext of the entity.

See POST (type)
See COMMENT (type)")

  (function op
    "Returns the original poster of the post.

This can be different from PAGE if the post is a share.

See POST (type)
See PAGE (type)")

  (type comment
    "Representation of a comment on a post.

See ENTITY (type)
See REPLY-TO
See POST
See REPLIES
See TIME
See TEXT
See DELETED-P
See HIDDEN-P
See AUTHOR
See INTERACTABLE-P
See EDITABLE-P
See HIDEABLE-P
See REPLY")

  (function reply-to
    "Returns the comment this comment is in reply to, if any.

See COMMENT (type)")

  (function post
    "Returns he post this entity was made in relation to.

See NOTIFICATION (type)
See POST (type)
See COMMENT (type)")

  (function replies
    "Returns a list of replies that were made to this comment.

See COMMENT (type)")

  (function deleted-p
    "Returns whether the post was marked as deleted.

See COMMENT (type)")

  (function hidden-p
    "Returns whether the current page has marked the post as hidden.

See COMMENT (type)")

  (function author
    "Returns the author page of the entity.

See NOTIFICATION (type)
See COMMENT (type)
See PAGE (type)")

  (function interactable-p
    "Returns whether the comment can be interacted with from the default page.

See COMMENT (type)")

  (function editable-p
    "Returns whether the comment can be edited.

See COMMENT (type)")

  (function hideable-p
    "Returns whether the comment can be hidden.

See COMMENT (type)")

  (type notification
    "Representation of a notification toast.

See KIND
See TIME
See AUTHOR
See POST
See SHARE-POST
See COMMENT")

  (function share-post
    "Returns the share post that was created, if any.

See NOTIFICATION (type)
See POST (type)")

  (function comment
    "Returns the comment that was created, if any.

See NOTIFICATION (type)
See COMMENT (type)"))
