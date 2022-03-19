;;;; middleware.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.middleware
  (:use :cl)
  (:local-nicknames (:error :cl-natter.error)
                    (:util :cl-natter.util))
  (:import-from :tiny-routes)
  (:export #:wrap-request-body
           #:wrap-request-json-body
           #:wrap-response-json-body
           #:wrap-condition))

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
       (tiny:header-response :content-type "application/json")
       (tiny:body-mapper-response #'jojo:to-json)))))


(defun wrap-condition (handler)
  (lambda (request)
    (handler-case (funcall handler request)
      (jojo:<jonathan-error> ()
        (tiny:bad-request (util:error-response "Unparsable JSON")))
      (error:natter-validation-error (e)
        (tiny:bad-request (util:error-response (format nil "~a" e)))))))
