;;;; cl-natter-test.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter-test
  (:use :cl :cl-natter :parachute)
  (:local-nicknames (:db :cl-natter.db)
                    (:middleware :cl-natter.middleware)
                    (:route :cl-natter.route)
                    (:user :cl-natter.controller.user)
                    (:space :cl-natter.controller.space)))

(in-package :cl-natter-test)

(define-test cl-natter)

(defun refresh-database ()
  (db:start-db (asdf:system-relative-pathname :cl-natter "db/test.db"))
  (dolist (name '("spaces" "messages" "users" "audit_log"))
    (db:execute (str:concat "DROP TABLE IF EXISTS " name))))

(defmacro with-fresh-database (&body body)
  `(let ((db:*db* (dbi:connect :sqlite3 :database-name "db/test.db")))
     (dolist (name '("spaces" "messages" "users" "audit_log"))
       (db:execute (str:concat "DROP TABLE IF EXISTS " name)))
     (db::create-tables)
     ,@body))

;;; util
(defun mock-request (&rest args &key method path body &allow-other-keys)
  (let* ((body (or body ""))
         (content-length (length body)))
    (append
     (list :request-method (or method :get)
           :path-info (or path "/")
           :headers (make-hash-table)
           :content-length content-length
           :raw-body (make-string-input-stream body))
     args)))

(defmacro matches (expected response)
  `(is equal ,expected ,response))

(define-test status-endpoint
  :parent cl-natter
  (let* ((request (mock-request :path "/status"))
         (response (funcall route::public-routes request)))
    (matches '(200 NIL (:|status| "live")) response)))

(define-test register-user
  :parent cl-natter
  (with-fresh-database
    (dotimes (i 10)
      (let* ((user:*hash-function* #'identity)
             (n (+ 1 i))
             (username (format nil "samplespace~d" n))
             (body `(:|username| ,username :|password| "sample-password"))
             (request (mock-request :path "/users" :method :post :json-body body))
             (response (funcall route::private-routes request))
             (uri (format nil "/users/~a" username)))
        (matches `(201 (:location ,uri) (:|username| ,username)) response)))))

(define-test create-space
  :parent cl-natter
  (with-fresh-database
    (dotimes (i 10)
      (let* ((n (+ 1 i))
             (name (format nil "samplespace~d" n))
             (body `(:|name| ,name :|owner| "anonymous"))
             (request (mock-request :path "/spaces" :method :post :json-body body))
             (response (funcall route::private-routes request))
             (uri (format nil "/spaces/~d" n)))
        (matches `(201 (:location ,uri) (:|name| ,name :|uri| ,uri)) response)))))

(define-test find-all-spaces
  :parent cl-natter
  (with-fresh-database
    (dolist (name '("samplespace1" "samplespace2"))
      (space:create-space name "anonymous"))
    (let* ((request (mock-request :path "/spaces"))
           (response (funcall route::private-routes request)))
      (matches '(200 nil (:|spaces|
                          ((:|space_id| 1 :|name| "samplespace1" :|owner| "anonymous")
                           (:|space_id| 2 :|name| "samplespace2" :|owner| "anonymous"))))
               response))))

(define-test post-message
  :parent cl-natter
  (with-fresh-database
    (space:create-space "samplespace1" "anonymous")
    (dotimes (i 10)
      (let* ((n (+ 1 i))
             (message (format nil "Test message ~d" n))
             (body `(:|author| "anonymous" :|message| ,message))
             (request (mock-request :path "/spaces/1/messages" :method :post :json-body body))
             (response (funcall route::private-routes request))
             (uri (format nil "/spaces/1/messages/~d" n)))
        (matches `(201 (:location ,uri) (:|uri| ,uri)) response)))))

(define-test find-all-space-messages
  :parent cl-natter
  (with-fresh-database
    (space:create-space "samplespace1" "anonymous")
    (dotimes (i 10)
      (let ((message (format nil "Test message ~d" (+ 1 i))))
        (space:post-message 1 "anonymous" message)))
    (let* ((request (mock-request :path "/spaces/1/messages"))
           (response (funcall route::private-routes request)))
      (is = 200 (tiny:response-status response))
      (is = 10 (length (getf (tiny:response-body response) :|messages|))))))

(define-test delete-message
  :parent cl-natter
  (with-fresh-database
    (space:create-space "samplespace1" "anonymous")
    (space:post-message 1 "anonymous" "Test message 1")
    (let* ((request (mock-request :path "/spaces/1/messages/1" :method :delete))
           (response (funcall route::private-routes request)))
      (matches '(200 nil (:|completed| t)) response))))
