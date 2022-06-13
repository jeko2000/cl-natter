;;;; session.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.session
  (:use :cl)
  (:local-nicknames (:a :alexandria)
                    (:sht :synchronized-hash-tables))
  (:import-from #:tiny-routes.middleware.cookie
                #:write-response-cookies)
  (:export #:*session-store*
           #:session-store
           #:read-session
           #:write-session
           #:delete-session
           #:read-session*
           #:write-session*
           #:delete-session*
           #:parse-session-id
           #:initialize-session
           #:make-session
           #:request-session
           #:session-response))

(in-package :cl-natter.session)

(defvar *session-cookie-name* "natter.sid")

(defvar *session-store* nil)

(defstruct session-store)

(defgeneric read-session (session-store key))
(defgeneric write-session (session-store key session))
(defgeneric delete-session (session-store key))

(defun read-session* (key)
  (read-session *session-store* key))

(defun write-session* (key session)
  (write-session *session-store* key session))

(defun delete-session* (key)
  (delete-session *session-store* key))

;; in-memory
(defstruct (memory-session-store (:include session-store))
  (table (sht:make-synchronized-hash-table :test 'equal)))

(defmethod read-session ((store memory-session-store) key)
  (sht:gethash key (memory-session-store-table store)))

(defmethod write-session ((store memory-session-store) key session)
  (setf (sht:gethash key (memory-session-store-table store)) session))

(defmethod delete-session ((store memory-session-store) key)
  (sht:remhash key (memory-session-store-table store)))

(defun initialize-session ()
  (setf *session-store* (make-memory-session-store)))

(defun parse-session-id (request)
  (a:when-let* ((cookies (tiny:request-get request :cookies))
                (sid-cookie (find *session-cookie-name* cookies
                                  :key #'cookie:cookie-name :test #'string-equal)))
    (cookie:cookie-value sid-cookie)))

;; session

(defun generate-session-id ()
  (format nil "~(~A~)" (uuid:make-v4-uuid)))

(defun make-session (&rest args &key sid (new-session t) &allow-other-keys)
  (let ((session (alexandria:plist-hash-table args)))
    (setf (gethash :sid session) (or sid (generate-session-id)))
    (setf (gethash :new-session session) new-session)
    session))

(defun request-session (request &key create-p)
  (or (tiny:request-get request :session)
      (and create-p (make-session))))

(defun session-response (response session)
  (let ((sid (and session (gethash :sid session))))
    (unless sid
      (return-from session-response response))
    (prog1 (if (gethash :new-session session)
               (write-response-cookies response (cookie:make-cookie :name *session-cookie-name* :value sid))
               response)
      (setf (gethash :new-session session) nil)
      (write-session* sid session))))
