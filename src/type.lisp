;;;; type.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.type
  (:use :cl)
  (:local-nicknames (:error :cl-natter.error)
                    (:util :cl-natter.util))
  (:import-from :ppcre)
  (:export #:check-username
           #:check-password
           #:check-space-id
           #:check-message-id
           #:check-space-name
           #:check-message-body
           #:check-lookback
           #:check-permissions
           #:check-limit))

(in-package :cl-natter.type)

(defvar *username-scanner*
  (ppcre:create-scanner "^[a-zA-Z][a-zA-Z0-9]{1,29}$"))

(defvar *permissions-scanner*
  (ppcre:create-scanner "^r?w?d?$"))

(defvar *password-min-length* 8)
(defvar *password-max-length* 255)

(defvar *space-name-min-length* 2)
(defvar *space-name-max-length* 255)

(defvar *message-body-min-length* 1)
(defvar *message-body-max-length* 1024)

(defun check-username (username)
  (unless (stringp username)
    (error:natter-validation-error "must provide username"))
  (unless (ppcre:scan *username-scanner* username)
    (error:natter-validation-error "invalid username"))
  username)

(defun check-password (password)
  (unless (stringp password)
    (error:natter-validation-error "must provide password"))
  (let ((length (length password)))
    (when (< length *password-min-length*)
      (error:natter-validation-error "password must be at least ~d characters" *password-min-length*))
    (when (> length *password-max-length*)
      (error:natter-validation-error "password must be at most ~d characters" *password-max-length*)))
  password)

(defun check-space-id (space-id)
  (let ((parsed-id (util:to-integer space-id)))
    (unless parsed-id
      (error:natter-validation-error "invalid space id ~a" space-id))
    (abs parsed-id)))

(defun check-message-id (message-id)
  (let ((parsed-id (util:to-integer message-id)))
    (unless parsed-id
      (error:natter-validation-error "invalid message id ~a" message-id))
    (abs parsed-id)))

(defun check-space-name (space-name)
  (unless (stringp space-name)
    (error:natter-validation-error "must provide space-name"))
  (let ((length (length space-name)))
    (when (< length *space-name-min-length*)
      (error:natter-validation-error "space-name must be at least ~d characters" *space-name-min-length*))
    (when (> length *space-name-max-length*)
      (error:natter-validation-error "space-name must be at most ~d characters" *space-name-max-length*)))
  space-name)

(defun check-message-body (message-body)
  (unless (stringp message-body)
    (error:natter-validation-error "must provide message-body"))
  (let ((length (length message-body)))
    (when (< length *message-body-min-length*)
      (error:natter-validation-error "message-body must be at least ~d characters" *message-body-min-length*))
    (when (> length *message-body-max-length*)
      (error:natter-validation-error "message-body must be at most ~d characters" *message-body-max-length*)))
  message-body)

(defun check-lookback (lookback)
  (let ((parsed-id (util:to-integer lookback)))
    (unless parsed-id
      (error:natter-validation-error "invalid lookback ~a" lookback))
    (abs parsed-id)))

(defun check-permissions (permissions)
  (unless (stringp permissions)
    (error:natter-validation-error "must provide permissions"))
  (unless (ppcre:scan *permissions-scanner* permissions)
    (error:natter-validation-error "invalid permissions"))
  permissions)

(defun check-limit (limit)
  (let ((parsed (util:to-integer limit)))
    (unless parsed
      (error:natter-validation-error "invalid limit ~a" limit))
    (abs parsed)))
