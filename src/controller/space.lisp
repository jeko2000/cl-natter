;;;; space.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.controller.space
  (:use :cl :cl-natter.type)
  (:local-nicknames (:db :cl-natter.db)
                    (:error :cl-natter.error)
                    (:util :cl-natter.util))
  (:export #:find-all-spaces
           #:create-space
           #:post-message
           #:find-message
           #:find-messages
           #:add-member))

(in-package :cl-natter.controller.space)

(defun find-all-spaces ()
  (db:query-all "SELECT * FROM spaces"));

(defun create-space (space-name owner)
  (setf space-name (check-space-name space-name)
        owner (check-username owner))
  (db:with-query-results (space-id)
      (db:query-one "INSERT INTO spaces(name, owner) VALUES(?,?) RETURNING space_id" space-name owner)
    space-id))

(defun post-message (space-id author message-body)
  (setf space-id (check-space-id space-id)
        author (check-username author)
        message-body (check-message-body message-body))
  (db:with-query-results (message-id)
      (db:query-one "INSERT INTO messages(space_id, created_at, author, message_text)
                     VALUES(?,current_timestamp,?,?) RETURNING message_id"
                    space-id author message-body)
    message-id))

(defun find-message (space-id message-id)
  (setf space-id (check-space-id space-id)
        message-id (check-message-id message-id))
  (db:ensure-one "SELECT space_id, message_id, author, created_at, message_text
                  FROM messages WHERE message_id = ? AND space_id = ?"
                 message-id space-id))

(defun find-messages (space-id &optional lookback)
  (if lookback
      (db:query-all "SELECT * FROM messages WHERE space_id = ? AND created_at =
                     DATE('now', ?)" space-id (check-lookback lookback))
      (db:query-all "SELECT * FROM messages")))

(defun add-member (space-id username permissions)
  (setf space-id (check-space-id space-id)
        username (check-username username)
        permissions (check-permissions permissions))
  (db:execute "INSERT INTO permissions(space_id, user_id, permissions)
               VALUES (?,?,?)" space-id username permissions))
