;;;; token.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.controller.token
  (:use :cl :cl-natter.type)
  (:local-nicknames (:log :cl-natter.logger)
                    (:token #:cl-natter.token))
  (:import-from :local-time
                #:timestamp+
                #:timestamp<
                #:now)
  (:export #:login
           #:wrap-token-auth))

(in-package :cl-natter.controller.token)

(defun login (request)
  (tiny:with-request (subject) request
    (log:info :token "Attempting to make token for user ~A" subject)
    (token:write-token*
     request
     (token:make-token :username subject
                       :expiry (timestamp+ (now) 10 :minute)))))

(defun wrap-token-auth (handler)
  (tiny:wrap-request-mapper
   handler
   (lambda (request)
     ;; read csrf from header
     (let* ((token-id (tiny:request-header request "x-csrf-token"))
            (token (and token-id (token:read-token* request token-id))))
       (if (and token (timestamp< (now) (token:token-expiry token)))
           (tiny:pipe request
             (tiny:request-append :subject (token:token-username token))
             (tiny:request-append :token token))
           request)))))
