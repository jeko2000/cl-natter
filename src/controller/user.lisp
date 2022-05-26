;;;; user.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.controller.user
  (:use :cl)
  (:local-nicknames (:db :cl-natter.db)
                    (:error :cl-natter.error))
  (:import-from :tiny-routes
                #:with-request)
  (:export #:register-user
           #:authenticate))

(in-package :cl-natter.controller.user)

(defvar *username-scanner*
  (ppcre:create-scanner "^[a-zA-Z][a-zA-Z0-9]{1,29}$"))

(defun check-username (username)
  (unless (stringp username)
    (error:natter-validation-error "must provide username"))
  (unless (ppcre:scan *username-scanner* username)
    (error:natter-validation-error "invalid username"))
  username)

(defun check-password (password)
  (unless (stringp password)
    (error:natter-validation-error "must provide password"))
  (when (< (length password) 8)
    (error:natter-validation-error "password must be at least 8 characters"))
  password)

(defun register-user (request)
  (with-request (json-body) request
    (let ((username (check-username (getf json-body :|username| "")))
          (password (check-password (getf json-body :|password| "")))
          (hash (bcrypt:encode (bcrypt:make-password password))))
      (db:execute "INSERT INTO users(user_id, pw_hash) VALUES(?,?)" username hash)
      (tiny:created (format nil "/users/~a" username) (list :|username| username)))))

(defun authenticate (username password)
  (check-username username)
  (check-password password)
  (let ((row (db:query-one "SELECT pw_hash FROM users WHERE user_id = ?" username)))
    (when (and row (bcrypt:password= password (getf row :|pw_hash|)))
      username)))
