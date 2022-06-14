;;;; token.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.token
  (:use :cl)
  (:local-nicknames (:a :alexandria)
                    (:crypt :cl-natter.crypt)
                    (:session :cl-natter.session))
  (:export #:token
           #:make-token
           #:token-id
           #:token-expiry
           #:token-username
           #:token-attributes
           #:*token-store*
           #:token-store
           #:write-token
           #:write-token*
           #:read-token
           #:read-token*
           #:cookie-token-store
           #:make-cookie-token-store
           #:initialize-token-store))

(in-package :cl-natter.token)

(defclass token ()
  ((id :initarg :id :accessor token-id)
   (expiry :initarg :expiry :accessor token-expiry)
   (username :initarg :username :accessor token-username)
   (attributes :initarg :attributes :accessor token-attributes)))

(defun make-token (&key id expiry username attributes)
  (make-instance 'token :id id :expiry expiry :username username :attributes attributes))

(defvar *token-store* nil)
(defstruct token-store)

(defgeneric write-token (store request token))
(defgeneric read-token (store request token-id))

(defun write-token* (request token)
  (write-token *token-store* request token))

(defun read-token* (request token-id)
  (read-token *token-store* request token-id))

;; cookie store

(defstruct (cookie-token-store (:include token-store)))

(defmethod write-token ((store cookie-token-store) request token)
  (with-slots (id username expiry attributes) token
    ;; invalidate session if present
    (a:when-let ((session (tiny:request-get request :session)))
      (session:delete-session* (gethash :sid session)))
    (let* ((session (session:make-session))
           (session-id (gethash :sid session))
           (token-id (crypt:base64-url-encode (crypt:sha256-string session-id))))
      (setf (token-id token) token-id
            (gethash :token session) token)
      (session:session-response
       (tiny:ok (list :|token_id| token-id))
       session))))

(defmethod read-token ((store cookie-token-store) request token-id)
  (a:when-let ((session (tiny:request-get request :session)))
    (let ((provided (crypt:base64-url-decode token-id))
          (computed (crypt:sha256-string (gethash :sid session))))
      (when (crypt:constant-time-equal provided computed)
        (gethash :token session)))))

(defun initialize-token-store ()
  (setf *token-store* (make-cookie-token-store)))
