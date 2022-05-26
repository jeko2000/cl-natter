;;;; db.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.db
  (:use :cl)
  (:local-nicknames (:error :cl-natter.error))
  (:import-from :cl-dbi)
  (:export #:*db*
           #:execute
           #:query-one
           #:ensure-one
           #:query-all
           #:stop-db
           #:start-db))

(in-package :cl-natter.db)

(defvar *db* nil
  "The Natter's API database.")

;;; util
(defun execute (sql &rest args)
  (dbi:do-sql *db* sql args))

(defun query (sql &rest args)
  (declare (inline query))
  (dbi:execute (dbi:prepare *db* sql) args))

(defun query-one (sql &rest args)
  (dbi:fetch (apply #'query sql args)))

(defun ensure-one (sql &rest args)
  (let ((rows (apply #'query-all sql args)))
    (unless rows
      (error:natter-not-found-error "No such resource found"))
    (when (cdr rows)
      (error:natter-not-found-error "Too many resources found"))
    (car rows)))

(defun query-all (sql &rest args)
  (let ((query (apply #'query sql args) ))
    (loop for row = (dbi:fetch query)
          while row collect row)))

(defun create-tables ()
  (execute "
    CREATE TABLE IF NOT EXISTS spaces (
      space_id       INTEGER PRIMARY KEY AUTOINCREMENT,
      name           VARCHAR(255) NOT NULL,
      owner          VARCHAR(30) NOT NULL)")
  (execute "
    CREATE TABLE IF NOT EXISTS messages (
      message_id     INTEGER PRIMARY KEY AUTOINCREMENT,
      space_id       INTEGER NOT NULL,
      author         VARCHAR(30) NOT NULL,
      message_text   VARCHAR(1024) NOT NULL,
      created_at     TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP)")
  (execute "CREATE INDEX IF NOT EXISTS message_timestamp_idx ON messages(created_at)")
  (execute "CREATE UNIQUE INDEX IF NOT EXISTS space_name_idx ON spaces(name)")
  ;; Add users table
  (execute "
    CREATE TABLE IF NOT EXISTS users (
      user_id VARCHAR(30) PRIMARY KEY,
      pw_hash VARCHAR(255) NOT NULL)")
  ;; Add audit log
  (execute "
    CREATE TABLE IF NOT EXISTS audit_log (
      audit_id   INTEGER PRIMARY KEY AUTOINCREMENT,
      method     VARCHAR(10) NOT NULL,
      path       VARCHAR(100) NOT NULL,
      user_id    VARCHAR(30),
      status     INTEGER,
      audit_time TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP)"))

(defun stop-db ()
  (when *db*
    (dbi:disconnect *db*)
    (setf *db* nil)
    t))

(defun start-db (database-name)
  (stop-db)
  (let ((name (etypecase database-name
                (string database-name)
                (pathname (namestring database-name)))))
    (setf *db* (dbi:connect :sqlite3 :database-name name)))
  (create-tables)
  *db*)
