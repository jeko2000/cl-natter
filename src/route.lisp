;;;; route.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.route
  (:use :cl)
  (:local-nicknames (:audit :cl-natter.controller.audit)
                    (:error :cl-natter.error)
                    (:middleware :cl-natter.middleware)
                    (:space :cl-natter.controller.space)
                    (:user :cl-natter.controller.user)
                    (:moderator :cl-natter.controller.moderator))
  (:import-from :tiny-routes
                #:define-routes
                #:define-route
                #:define-get
                #:define-post
                #:define-delete
                #:with-request
                #:pipe)
  (:export #:api-routes))

(in-package :cl-natter.route)

(define-routes public-routes
  (define-get "/status" () (tiny:ok (list :|status| "live")))
  (define-get "/logs" (request) (audit:read-audit-log request)))

(define-routes private-routes
  (define-post "/users" (request) (user:register-user request))
  ;; spaces
  (define-post "/spaces" (request) (space:create-space request))
  (define-post "/spaces/:space_id/messages" (request) (space:post-message request))
  (define-get "/spaces/:space_id/messages/:message_id" (request) (space:read-message request))
  (define-get "/spaces/:space_id/messages" (request) (space:find-messages request))
  (define-post "/spaces/:space_id/members" (request) (space:add-member request))
  (define-delete "/spaces/:space_id/messages/:message_id" (request) (moderator:delete-post request)))

(define-routes api-routes
  (pipe (tiny:routes public-routes (middleware:wrap-require-authentication private-routes))
    (middleware:wrap-require-json-content-type)
    (middleware:wrap-condition)
    (middleware:wrap-audit-log)
    (middleware:wrap-auth)
    (middleware:wrap-rate-limiter)
    (middleware:wrap-response-json-body)
    (tiny:wrap-query-parameters)
    ;; We should keep this middleware towards the end to prevent
    ;; spurious hunchentoot errors about closed response streams
    (middleware:wrap-request-json-body)
    (middleware:wrap-sane-headers))

  ;; catch all route
  (define-route ()
    (tiny:not-found "NOT_FOUND")))
