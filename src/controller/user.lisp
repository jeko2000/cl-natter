;;;; user.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.controller.user
  (:use :cl)
  (:local-nicknames (:db :cl-natter.db)
                    (:error :cl-natter.error))
  (:export #:register-user
           #:authenticate))

(in-package :cl-natter.controller.user)

(defvar *username-scanner*
  (ppcre:create-scanner "^[a-zA-Z][a-zA-Z0-9]{1,29}$"))

(defun register-user (username password)
  (unless (ppcre:scan *username-scanner* username)
    (error:natter-validation-error "invalid username"))
  (when (< (length password) 8)
    (error:natter-validation-error "password must be at least 8 characters"))
  (let ((hash (bcrypt:encode (bcrypt:make-password password))))
    (db:execute "INSERT INTO users(user_id, pw_hash) VALUES(?,?)" username hash)
    username))

(defun authenticate (username password)
  (unless (ppcre:scan *username-scanner* username)
    (error:natter-validation-error "invalid username"))
  (let ((row (db:query-one "SELECT pw_hash FROM users WHERE user_id = ?" username)))
    (when (and row (bcrypt:password= password (getf row :|pw_hash|)))
      username)))
