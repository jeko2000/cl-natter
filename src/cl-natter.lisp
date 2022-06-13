;;;; cl-natter.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter
  (:use :cl)
  (:local-nicknames (:db :cl-natter.db)
                    (:log :cl-natter.logger)
                    (:server :cl-natter.server)
                    (:session :cl-natter.session))
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
    (server:stop-http-server)
    (setf *app-status* :stopped))
  *app-status*)

(defun start-app (&key (db-path-name "db/cl-natter.db") force-restart-p)
  (when (or (stopped-p) force-restart-p)
    (stop-app)
    (log:initialize-logger)
    (session:initialize-session)
    (db:start-db (asdf:system-relative-pathname :cl-natter db-path-name))
    (server:start-http-server)
    (setf *app-status* :running))
  *app-status*)

(start-app :force-restart-p t)
