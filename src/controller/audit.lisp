;;;; audit.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.controller.audit
  (:use :cl)
  (:local-nicknames (:db :cl-natter.db))
  (:import-from :tiny
                #:with-request
                #:response-status)
  (:export #:audit-request-start
           #:audit-request-end
           #:read-audit-log))

(in-package :cl-natter.controller.audit)

(defun audit-request-start (request)
  (with-request (request-method path-info subject) request
    (let ((row (db:query-one
                "INSERT INTO audit_log(method,path,user_id) VALUES (?,?,?) RETURNING audit_id"
                (symbol-name request-method) path-info subject)))
      (getf row :|audit_id|))))

(defun audit-request-end (request response)
  (with-request (request-method path-info subject) request
    (let* ((status (response-status response))
           (row (db:query-one
                 "INSERT INTO audit_log(method,path,user_id,status)
                 VALUES (?,?,?,?) RETURNING audit_id"
                 (symbol-name request-method) path-info subject status)))
      (getf row :|audit_id|))))

(defun read-audit-log (&key (lookback-hours 1) (limit 20))
  (db:query-all "SELECT * FROM audit_log WHERE audit_time>=DATETIME('now', ?) LIMIT ?"
                (format nil "-~D hour" lookback-hours) limit))
