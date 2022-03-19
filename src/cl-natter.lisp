;;;; cl-natter.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter
  (:use :cl)
  (:local-nicknames (:db :cl-natter.db))
  (:export #:start-app
           #:stop-app))

(in-package :cl-natter)

(deftype app-status () '(member :running :stopped))

(defvar *app-status* :stopped)

(defun running-p ()
  (declare (inline running-p))
  (eq *app-status* :running))

(defun stopped-p ()
  (declare (inline stopped-p))
  (not (running-p)))

(defun stop-app ()
  (when (running-p)
    (db:stop-db)
    (setf *app-status* :stopped))
  *app-status*)

(defun start-app (&optional force-restart-p)
  (when (or (stopped-p) force-restart-p)
    (stop-app)
    (db:start-db (asdf:system-relative-pathname :cl-natter "db/cl-natter.db"))
    (setf *app-status* :running))
  *app-status*)
