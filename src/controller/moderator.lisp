;;;; moderator.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.controller.moderator
  (:use :cl)
  (:local-nicknames (:db :cl-natter.db))
  (:import-from :tiny-routes
                #:with-request)
  (:export #:delete-post))

(in-package :cl-natter.controller.moderator)

(defun check-space-id (space-id)
  (parse-integer (or space-id "") :junk-allowed t))

(defun delete-post (request)
  (with-request (path-parameters) request
    (let ((space-id (parse-integer (getf path-parameters :|space_id| 0) :junk-allowed t))
          (message-id (parse-integer (getf path-parameters :|message_id| 0) :junk-allowed t)))
      (db:execute "DELETE FROM messages WHERE space_id = ? AND message_id = ?"
                  space-id message-id)
      (tiny:ok (list :|completed| t)))))
