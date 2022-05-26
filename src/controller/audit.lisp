;;;; audit.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.controller.audit
  (:use :cl)
  (:local-nicknames (:db :cl-natter.db)
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

(defun check-lookback-hours (lookback-hours)
  (or (parse-integer (or lookback-hours "") :junk-allowed t)
      +default-lookback-hours+))

(defun check-limit (limit)
  (or (parse-integer (or limit "") :junk-allowed t)
      +default-limit+))

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

(defun read-audit-log (request)
  (with-request (query-parameters) request
    (let ((lookback-hours (check-lookback-hours (getf query-parameters :|lookback_hours|)))
          (limit (check-limit (getf query-parameters :|limit|))))
      (tiny:ok (read-audit-log* :lookback-hours lookback-hours :limit limit)))))

(defun record-to-json (row)
  (list
   :|id| (getf row :|audit_id|)
   :|method| (getf row :|method|)
   :|path| (getf row :|path|)
   :|status| (or (getf row :|status|) :null)
   :|user| (or (getf row :|user_id|) :null)
   :|time| (getf row :|audit_time|)))
