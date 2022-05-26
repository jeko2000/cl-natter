;;;; space.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.controller.space
  (:use :cl)
  (:local-nicknames (:db :cl-natter.db)
                    (:error :cl-natter.error))
  (:import-from :tiny-routes
                #:with-request)
  (:export #:create-space
           #:post-message
           #:read-message
           #:find-messages
           #:add-member))

(in-package :cl-natter.controller.space)

(alexandria:define-constant +default-message-lookback-seconds+
    (* 60 60 24))

(defvar *username-scanner*
  (ppcre:create-scanner "^[a-zA-Z][a-zA-Z0-9]{1,29}$"))

(defvar *permissions-scanner*
  (ppcre:create-scanner "^r?w?d?$"))

(defun check-space-name (space-name)
  (when (> (length (or space-name "")) 255)
    (error:natter-validation-error "space name too long"))
  space-name)

(defun check-username (username)
  (unless (ppcre:scan *username-scanner* (or username ""))
    (error:natter-validation-error "invalid username"))
  username)

(defun check-message (message)
  (when (> (length (or message "")) 1024)
    (error:natter-validation-error "message is too long"))
  message)

(defun check-message-id (message-id)
  (let ((parsed-id (parse-integer (or message-id "") :junk-allowed t)))
    (unless parsed-id
      (error:natter-validation-error "invalid message id ~a" message-id))
    parsed-id))

(defun check-space-id (space-id)
  (let ((parsed-id (parse-integer (or space-id "") :junk-allowed t)))
    (unless parsed-id
      (error:natter-validation-error "invalid space id ~a" space-id))
    parsed-id))

(defun check-message-lookback (lookback)
  (let ((message-lookback (or (parse-integer (or lookback "") :junk-allowed t)
                              +default-message-lookback-seconds+)))
    (format nil "-~d seconds" message-lookback)))

(defun check-permissions (permissions)
  (unless (ppcre:scan *permissions-scanner* (or permissions ""))
    (error:natter-validation-error "invalid permissions"))
  permissions)

(defun create-space (request)
  (with-request (json-body) request
    (let* ((space-name (check-space-name (getf json-body :|name|)))
           (owner (check-username (getf json-body :|owner|)))
           (row (db:query-one "INSERT INTO spaces(name, owner) VALUES(?,?)
                               RETURNING space_id" space-name owner))
           (space-id (getf row :|space_id|))
           (uri (format nil "/spaces/~a" space-id)))
      (tiny:created uri (list :|name| space-name :|uri| uri)))))

(defun post-message (request)
  (with-request (path-parameters json-body) request
    (let* ((space-id (check-space-id (getf path-parameters :|space_id|)))
           (author (check-username (getf json-body :|author|)))
           (message (check-message (getf json-body :|message|)))

           (row (db:query-one "INSERT INTO messages(space_id, created_at, author, message_text)
                              VALUES(?,current_timestamp,?,?) RETURNING message_id"
                              space-id author message))
           (message-id (getf row :|message_id|))
           (uri (format nil "/spaces/~a/messages/~a" space-id message-id)))
      (tiny:created uri (list :|uri| uri)))))

(defun read-message (request)
  (with-request (path-parameters) request
    (let* ((space-id (check-space-id (getf path-parameters :|space_id|)))
           (message-id (check-message-id (getf path-parameters :|message_id|)))
           (message (db:ensure-one "SELECT space_id, message_id, author, created_at, msg_test
                                    FROM messages WHERE message_id = ? AND space_id = ?"
                                   message-id space-id)))
      (tiny:ok (list :|message| message)))))

(defun find-messages (request)
  (with-request (path-parameters query-parameters) request
    (let* ((space-id (check-space-id (getf path-parameters :|space_id|)))
           (since (check-message-lookback (getf query-parameters :|since|)))
           (messages (db:query-all "SELECT message_id FROM messages WHERE space_id = ?
                                    AND created_at = DATE('now', ?)" space-id since))
           (messages (mapcar (message-to-url-function space-id) messages)))
      (tiny:ok messages))))

(defun add-member (request)
  (with-request (json-body path-parameters) request
    (let ((space-id (check-space-id (getf path-parameters :|space_id|)))
          (username (check-username (getf json-body :|username|)))
          (permissions (check-permissions (getf json-body :|permissions|))))
      (db:execute "INSERT INTO permissions(space_id, user_id, permissions)
                   VALUES (?,?,?)" space-id username permissions)
      (tiny:ok (list :|username| username :|permissions permissions|)))))
