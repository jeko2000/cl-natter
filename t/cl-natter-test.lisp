;;;; cl-natter-test.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter-test
  (:use :cl :cl-natter :parachute)
  (:local-nicknames (:a :alexandria)
                    (:db :cl-natter.db)
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
(defun mock-request (&rest args &key method path body basic-auth content-type cookie csrf-token &allow-other-keys)
  (let* ((body (or body ""))
         (content-length (length body))
         (headers (make-hash-table :test 'equal))
         (content-type (or content-type "application/json")))
    (when basic-auth
      (setf (gethash "authorization" headers)
            (format nil "Basic ~A" (apply #'basic-authentication-header basic-auth))))
    (when cookie
      (setf (gethash "cookie" headers)
            (cl-cookie:write-cookie-header cookie)))
    (when csrf-token
      (setf (gethash "x-csrf-token" headers) csrf-token))
    (append
     (list :request-method (or method :get)
           :path-info (or path "/")
           :headers headers
           :content-length content-length
           :raw-body (make-string-input-stream body)
           :content-type content-type)
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
             (response (funcall route::public-routes request))
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

(defun basic-authentication-header (username password)
  (base64:string-to-base64-string
   (format nil "~A:~A" username password)))

(defun parse-session-cookie (response)
  (a:when-let ((set-cookie-string (tiny:response-header response :set-cookie)))
    (cl-cookie:parse-set-cookie-header set-cookie-string "" "")))

(define-test csrf
  :parent cl-natter
  (with-fresh-database
    (user:register-user "sampleuser1" "samplepassword1")
    (let* ((request (mock-request :method :post :path "/login" :basic-auth (list "sampleuser1" "samplepassword1")))
           (response (funcall route:app-routes request))
           (session-cookie (cl-cookie:parse-set-cookie-header (tiny:response-header response :set-cookie) "" ""))
           (body (jojo:parse (tiny:response-body response)))
           (token-id (getf body :|token_id|)))
      (is = 200 (tiny:response-status response))
      (let ((request0 (mock-request :method :get :path "/spaces" :cookie session-cookie :csrf-token token-id))
            (request1 (mock-request :method :get :path "/spaces" :cookie session-cookie))
            (request2 (mock-request :method :get :path "/spaces" :csrf-token token-id)))
        (matches '(200
                   (:CONTENT-TYPE "application/json;charset=utf-8" :SERVER "cl-natter"
                    :X-CONTENT-TYPE-OPTIONS "nosniff" :X-FRAME-OPTIONS "DENY" :X-XSS-PROTECTION
                    "0" :CACHE-CONTROL "no-store" :PRAGMA "no-cache" :CONTENT-SECURITY-POLICY
                    "default-src 'none'; frame-ancestors 'none'; sandbox")
                   ("{\"spaces\":[]}"))
                 (funcall route:app-routes request0))
        (matches '(401
                   (:CONTENT-TYPE "application/json;charset=utf-8" :WWW-AUTHENTICATE
                    "Basic realm=\"/\", charset=\"UTF-8\"" :SERVER "cl-natter"
                    :X-CONTENT-TYPE-OPTIONS "nosniff" :X-FRAME-OPTIONS "DENY" :X-XSS-PROTECTION
                    "0" :CACHE-CONTROL "no-store" :PRAGMA "no-cache" :CONTENT-SECURITY-POLICY
                    "default-src 'none'; frame-ancestors 'none'; sandbox")
                   ("{\"error\":\"Unauthorized\"}"))
                 (funcall route:app-routes request1))
        (matches '(401
                   (:CONTENT-TYPE "application/json;charset=utf-8" :WWW-AUTHENTICATE
                    "Basic realm=\"/\", charset=\"UTF-8\"" :SERVER "cl-natter"
                    :X-CONTENT-TYPE-OPTIONS "nosniff" :X-FRAME-OPTIONS "DENY" :X-XSS-PROTECTION
                    "0" :CACHE-CONTROL "no-store" :PRAGMA "no-cache" :CONTENT-SECURITY-POLICY
                    "default-src 'none'; frame-ancestors 'none'; sandbox")
                   ("{\"error\":\"Unauthorized\"}"))
                 (funcall route:app-routes request2))))))
