;;;; moderator.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.controller.moderator
  (:use :cl :cl-natter.type)
  (:local-nicknames (:db :cl-natter.db)
                    (:error :cl-natter.error)
                    (:log :cl-natter.logger)
                    (:util :cl-natter.util))
  (:export #:delete-post))

(in-package :cl-natter.controller.moderator)

;; TODO: Check if record was actually deleted (i.e., record may not exist)
(defun delete-post (space-id message-id)
  (setf space-id (check-space-id space-id)
        message-id (check-message-id message-id))
  (log:info :moderator "Attempting to delete post for space-id ~A and message id ~A"
            space-id message-id)
  (db:execute "DELETE FROM messages WHERE space_id = ? AND message_id = ?"
              space-id message-id))
