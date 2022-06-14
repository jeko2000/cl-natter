;;;; middleware.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.middleware
  (:use :cl)
  (:local-nicknames (:audit :cl-natter.controller.audit)
                    (:error :cl-natter.error)
                    (:log :cl-natter.logger)
                    (:rate-limiter :cl-natter.rate-limiter)
                    (:session :cl-natter.session)
                    (:user :cl-natter.controller.user)
                    (:util :cl-natter.util))
  (:import-from :tiny-routes
                #:wrap-request-mapper
                #:wrap-response-mapper
                #:request-append
                #:pipe
                #:with-request
                #:request-body
                #:wrap-request-body)
  (:export
   #:with-json
   #:wrap-request-body
   #:wrap-request-json-body
   #:wrap-response-json-body
   #:wrap-condition
   #:wrap-sane-headers
   #:wrap-require-json-content-type
   #:wrap-rate-limiter
   #:wrap-auth
   #:wrap-audit-log
   #:wrap-require-authentication
   #:wrap-logging
   #:wrap-session-request))

(in-package :cl-natter.middleware)

(defun wrap-request-json-body (handler)
  (wrap-request-body
   (wrap-request-mapper
    handler
    (lambda (request)
      (let* ((request-body (or (request-body request) ""))
             (json-body (jojo:parse request-body)))
        (request-append request :json-body json-body))))))

(defun wrap-response-json-body (handler)
  (wrap-response-mapper
   handler
   (lambda (response)
     (tiny:clone-response
      response
      :headers (append (list :content-type "application/json;charset=utf-8")
                       (tiny:response-headers response))
      :body (jojo:to-json (tiny:response-body response))))))

(defun wrap-condition (handler)
  (lambda (request)
    (handler-case (funcall handler request)
      (jojo:<jonathan-error> ()
        (tiny:bad-request (util:error-response "Unparsable JSON")))
      (error:natter-validation-error (e)
        (tiny:bad-request (util:error-response (format nil "~a" e))))
      (error:natter-not-found-error ()
        (tiny:not-found (util:error-response "No such resource found"))))))

(defvar sane-headers
  (list
   :server "cl-natter"
   :x-content-type-options "nosniff"
   :x-frame-options "DENY"
   :x-xss-protection "0"
   :cache-control "no-store"
   :pragma "no-cache"
   :content-security-policy "default-src 'none'; frame-ancestors 'none'; sandbox"))

(defun wrap-sane-headers (handler)
  (wrap-response-mapper
   handler
   (lambda (response)
     (tiny:clone-response response
                          :headers (append (tiny:response-headers response) sane-headers)))))

(defun wrap-require-json-content-type--internal (handler)
  (lambda (request)
    (let ((request-method (tiny:request-method request)))
      (if (and (eq request-method :post)
               (not (str:starts-with-p "application/json" (tiny:content-type request ""))))
          (tiny:make-response :status 415 :body (util:error-response "Unsupported media type"))
          (funcall handler request)))))

(defun wrap-require-json-content-type (handler)
  (tiny:wrap-post-match-middleware handler #'wrap-require-json-content-type--internal))

(defun wrap-rate-limiter (handler)
  (declare (ignorable handler))
  (lambda (request)
    (if (rate-limiter:try-acquire)
        (funcall handler request)
        (tiny:make-response :status 429
                            :headers '(:retry-after "2")
                            :body (util:error-response "Too many requests")))))

(defun wrap-auth (handler)
  (wrap-request-mapper
   handler
   (lambda (request)
     (let ((authorization (tiny:request-header request "authorization")))
       (multiple-value-bind (username password) (util:parse-basic-authorization authorization)
         (if (and username password (user:authenticate username password))
             (request-append request :subject username)
             request))))))

(defun wrap-audit-log (handler)
  (lambda (request)
    (let* ((audit-id (audit:audit-request-start request))
           (response (funcall handler (request-append request :audit-id audit-id))))
      (audit:audit-request-end request response)
      response)))

(defvar *default-www-authenticate-header-value*
  "Basic realm=\"/\", charset=\"UTF-8\"")

(defun wrap-require-authentication--internal (handler)
  (declare (ignorable handler))
  (lambda (request)
    (uiop:if-let ((subject (tiny:request-get request :subject)))
      (funcall handler request)
      (tiny:make-response :status 401 :headers (list :www-authenticate *default-www-authenticate-header-value*)
                          :body (util:error-response "Unauthorized")))))

(defun wrap-require-authentication (handler)
  (tiny:wrap-post-match-middleware handler #'wrap-require-authentication--internal))

(defun wrap-logging (handler)
  (lambda (request)
    (let ((start (get-internal-real-time)))
      (tiny:with-request (request-method path-info request-body) request
        (log:info :middleware "Handling HTTP ~a to ~s, body ~s" request-method path-info request-body)
        (let* ((response (funcall handler request))
               (duration (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
          (log:info :middleware "Handled HTTP ~a to ~s after ~fs with status ~a"
                    request-method path-info duration (tiny:response-status response))
          response)))))

(defun wrap-session-request (handler &key (session-id-parser #'session:parse-session-id))
  (wrap-request-mapper
   handler
   (lambda (request)
     (log:info :middleware "Attempting to parse session id from request")
     (let* ((sid (funcall session-id-parser request))
            (session (and sid (session:read-session* sid))))
       (log:info :middleware "Found sid ~A" sid)
       (pipe request
         (request-append :session session)
         (request-append :session-id sid))))))
