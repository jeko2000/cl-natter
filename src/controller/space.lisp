;;;; space.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.controller.space
  (:use :cl)
  (:local-nicknames (:db :cl-natter.db))
  (:export #:create-space))

(in-package :cl-natter.controller.space)

(defun create-space (space-name owner)
  (let ((row (db:query-one
              "INSERT INTO spaces(name, owner) VALUES(?,?) RETURNING space_id"
              space-name owner)))
    (getf row :|space_id|)))
