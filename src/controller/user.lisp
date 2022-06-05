;;;; user.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.controller.user
  (:use :cl :cl-natter.type)
  (:local-nicknames (:db :cl-natter.db)
                    (:error :cl-natter.error))
  (:import-from :tiny-routes
                #:with-request)
  (:export #:*hash-function*
           #:register-user
           #:authenticate))

(in-package :cl-natter.controller.user)

(defvar *username-scanner*
  (ppcre:create-scanner "^[a-zA-Z][a-zA-Z0-9]{1,29}$"))

(defun bcrypt-hash (password)
  (bcrypt:encode (bcrypt:make-password password)))

(defvar *hash-function* #'bcrypt-hash)

(defun register-user (username password)
  (check-username username)
  (check-password password)
  (let ((hash (funcall *hash-function* password)))
    (db:execute "INSERT INTO users(user_id, pw_hash) VALUES(?,?)" username hash)
    username))

(defun authenticate (username password)
  (check-username username)
  (check-password password)
  (let ((row (db:query-one "SELECT pw_hash FROM users WHERE user_id = ?" username)))
    (when (and row (bcrypt:password= password (getf row :|pw_hash|)))
      username)))
