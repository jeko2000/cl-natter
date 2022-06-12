;;;; audit.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.controller.audit
  (:use :cl :cl-natter.type)
  (:local-nicknames (:db :cl-natter.db)
                    (:log :cl-natter.logger)
                    (:util :cl-natter.util))
  (:import-from :tiny
                #:with-request
                #:response-status)
  (:export #:audit-request-start
           #:audit-request-end
           #:read-audit-log))

(in-package :cl-natter.controller.audit)

(alexandria:define-constant +default-lookback-hours+ 1)
(alexandria:define-constant +default-limit+ 20)

(defun audit-request-start (request)
  (with-request (request-method path-info subject) request
    (db:with-query-results (audit-id)
        (db:query-one "INSERT INTO audit_log(method,path,user_id) VALUES (?,?,?) RETURNING audit_id"
                      (symbol-name request-method) path-info subject)
      audit-id)))

(defun audit-request-end (request response)
  (with-request (request-method path-info subject) request
    (db:with-query-results (audit-id)
        (db:query-one "INSERT INTO audit_log(method,path,user_id,status) VALUES (?,?,?,?) RETURNING audit_id"
                      (symbol-name request-method) path-info subject (response-status response))
      audit-id)))

(defun read-audit-log (&key lookback-hours limit)
  (setf lookback-hours (check-lookback (or lookback-hours +default-lookback-hours+))
        limit (check-limit (or limit +default-limit+)))
  (log:info :audit "Attempting to read audit log with lookback ~Ahrs and limit ~A" lookback-hours limit)
  (loop for row in (db:query-all "SELECT * FROM audit_log WHERE audit_time>=DATETIME('now', ?) LIMIT ?"
                                 (format nil "-~D hour" lookback-hours) limit)
        collect (record-to-json row)))

(defun record-to-json (row)
  (list
   :|id| (getf row :|audit_id|)
   :|method| (getf row :|method|)
   :|path| (getf row :|path|)
   :|status| (or (getf row :|status|) :null)
   :|user| (or (getf row :|user_id|) :null)
   :|time| (getf row :|audit_time|)))
