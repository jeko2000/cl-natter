;;;; middleware.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.middleware
  (:use :cl)
  (:local-nicknames (:error :cl-natter.error)
                    (:rate-limiter :cl-natter.rate-limiter)
                    (:user :cl-natter.controller.user)
                    (:util :cl-natter.util))
  (:import-from :tiny-routes)

  (:export #:wrap-request-body
           #:wrap-request-json-body
           #:wrap-response-json-body
           #:wrap-condition
           #:wrap-sane-headers
           #:wrap-require-json-content-type
           #:wrap-rate-limiter
           #:wrap-auth))

(in-package :cl-natter.middleware)

(defun wrap-request-body (handler)
  (tiny:wrap-request-mapper
   handler
   (lambda (request)
     (tiny:with-request (request-method raw-body) request
       (let ((body (if (member request-method '(:patch :post :put))
                       (tiny-routes.middleware::read-stream-to-string raw-body)
                       "")))
         (tiny:request-append request :request-body body))))))

(defun wrap-request-json-body (handler)
  (wrap-request-body
   (tiny:wrap-request-mapper
    handler
    (lambda (request)
      (let* ((request-body (tiny:request-body request))
             (json-body (jojo:parse request-body)))
        (tiny:request-append request :json-body json-body))))))

(defun wrap-response-json-body (handler)
  (tiny:wrap-response-mapper
   handler
   (lambda (response)
     (tiny:pipe response
       (tiny:header-response :content-type "application/json;charset=utf-8")
       (tiny:body-mapper-response #'jojo:to-json)))))

(defun wrap-condition (handler)
  (lambda (request)
    (handler-case (funcall handler request)
      (jojo:<jonathan-error> ()
        (tiny:bad-request (util:error-response "Unparsable JSON")))
      (error:natter-validation-error (e)
        (tiny:bad-request (util:error-response (format nil "~a" e)))))))

(defun wrap-sane-headers (handler)
  (tiny:wrap-response-mapper
   handler
   (lambda (response)
     (tiny:pipe response
       (tiny:header-response :server "cl-natter")
       (tiny:header-response :x-content-type-options  "nosniff")
       (tiny:header-response :x-frame-options  "DENY")
       (tiny:header-response :x-xss-protection "0")
       (tiny:header-response :cache-control "no-store")
       (tiny:header-response :pragma "no-cache")
       (tiny:header-response :content-security-policy
                             "default-src 'none'; frame-ancestors 'none'; sandbox")))))

(defun wrap-require-json-content-type (handler)
  (declare (ignorable handler))
  (lambda (request)
    (let ((request-method (tiny:request-method request)))
      (if (and (eq request-method :post)
               (not (str:starts-with-p "application/json" (tiny:content-type request ""))))
          (tiny:make-response :status 415 :body (util:error-response "Unsupported media type"))
          (funcall handler request)))))

(defun wrap-rate-limiter (handler)
  (declare (ignorable handler))
  (lambda (request)
    (if (rate-limiter:try-acquire)
        (funcall handler request)
        (tiny:make-response :status 429
                            :headers '(:retry-after "2")
                            :body (util:error-response "Too many requests")))))

(defun wrap-auth (handler)
  (tiny:wrap-request-mapper
   handler
   (lambda (request)
     (let ((authorization (tiny:request-header request "authorization")))
       (multiple-value-bind (username password) (util:parse-basic-authorization authorization)
         (if (and username password (user:authenticate username password))
             (tiny:request-append request :subject username)
             request))))))
