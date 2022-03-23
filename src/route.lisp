;;;; route.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.route
  (:use :cl)
  (:local-nicknames (:space :cl-natter.controller.space)
                    (:user :cl-natter.controller.user)
                    (:middleware :cl-natter.middleware)
                    (:error :cl-natter.error))
  (:import-from :tiny-routes
                #:define-routes
                #:define-route
                #:define-get
                #:define-post
                #:with-request
                #:pipe)
  (:export #:api-routes))

(in-package :cl-natter.route)

(define-routes user-routes

  (define-get "/status" ()
    (tiny:ok (list :|status| "live")))

  (define-post "/users" (request)
    (with-request (json-body) request
      (let ((username (getf json-body :|username| ""))
            (password (getf json-body :|password| "")))
        (user:register-user username password)
        (tiny:created (format nil "/users/~a" username) (list :|username| username))))))

(define-routes space-routes
  (define-post "/spaces" (request)
    (with-request (subject json-body) request
      (let* ((space-name (getf json-body :|name| ""))
             (owner (getf json-body :|owner| ""))
             (space-id (space:create-space space-name owner subject))
             (uri (format nil "/spaces/~a" space-id)))
        (tiny:created uri (list :|name| space-name :|uri| uri))))))

(define-routes api-routes
  (pipe (tiny:routes space-routes user-routes)
    (middleware:wrap-require-json-content-type)
    (middleware:wrap-condition)
    (middleware:wrap-rate-limiter)
    (middleware:wrap-response-json-body)
    (middleware:wrap-auth)
    ;; We should keep this middleware towards the end to prevent
    ;; spurious hunchentoot errors about closed response streams
    (middleware:wrap-request-json-body)
    (middleware:wrap-sane-headers))

  ;; catch all route
  (define-route ()
    (tiny:not-found "NOT_FOUND")))
