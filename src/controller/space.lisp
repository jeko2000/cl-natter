;;;; space.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.controller.space
  (:use :cl)
  (:local-nicknames (:db :cl-natter.db)
                    (:error :cl-natter.error))
  (:export #:create-space))

(in-package :cl-natter.controller.space)

(defvar *owner-scanner*
  (ppcre:create-scanner "^[a-zA-Z][a-zA-Z0-9]{1,29}$"))

(defun create-space (space-name owner subject)
  (unless (string= owner subject)
    (error:natter-validation-error "owner must match authenticated user"))
  (when (> (length space-name) 255)
    (error:natter-validation-error "space name too long"))
  (unless (ppcre:scan *owner-scanner* owner)
    (error:natter-validation-error "invalid username"))
  (let ((row (db:query-one
              "INSERT INTO spaces(name, owner) VALUES(?,?) RETURNING space_id"
              space-name owner)))
    (getf row :|space_id|)))
