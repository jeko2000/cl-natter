;;;; route.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.route
  (:use :cl)
  (:local-nicknames (:audit :cl-natter.controller.audit)
                    (:error :cl-natter.error)
                    (:middleware :cl-natter.middleware)
                    (:space :cl-natter.controller.space)
                    (:user :cl-natter.controller.user)
                    (:moderator :cl-natter.controller.moderator)
                    (:util :cl-natter.util))
  (:import-from :cl-natter.middleware
                #:with-json)
  (:import-from :cl-natter.util
                #:with-request-payload
                #:with-path-parameters
                #:with-query-parameters)
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
  (define-post "/echo" (request) (tiny:ok (util:map-plist-values request #'util:to-string)))
  (define-get "/logs" (request) (audit:read-audit-log request)))

(define-routes private-routes
  ;; register-user
  (define-post "/users" (request)
    (with-request-payload (username password) request
      (user:register-user username password)
      (let ((uri (str:concat "/users/" username)))
        (tiny:created uri (list :|username| username)))))

  ;; create-space
  (define-post "/spaces" (request)
    (with-request-payload (name owner) request
      (let* ((space-id (space:create-space name owner))
             (uri (format nil "/spaces/~a" space-id)))
        (tiny:created uri (list :|name| name :|uri| uri)))))

  ;; find all spaces
  (define-get "/spaces" ()
    (let ((spaces (space:find-all-spaces)))
      (tiny:ok (list :|spaces| spaces))))

  ;; post message
  (define-post "/spaces/:space-id/messages" (request)
    (with-path-parameters (space-id) request
      (with-request-payload (author message) request
        (let* ((message-id (space:post-message space-id author message))
               (uri (format nil "/spaces/~a/messages/~a" space-id message-id)))
          (tiny:created uri (list :|uri| uri))))))

  ;; find message
  (define-get "/spaces/:space-id/messages/:message-id" (request)
    (with-path-parameters (space-id message-id) request
      (let ((message (space:find-message space-id message-id)))
        (tiny:ok (list :|message| message)))))

  ;; find all messages in space
  (define-get "/spaces/:space-id/messages" (request)
    (with-path-parameters (space-id) request
      (with-query-parameters (since) request
        (let ((messages (space:find-messages space-id since)))
          (tiny:ok (list :|messages| messages))))))

  ;; add new member
  (define-post "/spaces/:space-id/members" (request)
    (with-path-parameters (space-id) request
      (with-request-payload (username permissions) request
        (space:add-member space-id username permissions)
        (tiny:ok (list :|username| username :|permissions permissions|)))))

  ;; delete message
  (define-delete "/spaces/:space-id/messages/:message-id" (request)
    (with-path-parameters (space-id message-id) request
      (moderator:delete-post space-id message-id)
      (tiny:ok (list :|completed| t)))))

(define-routes api-routes
  (pipe (tiny:routes public-routes
                     (middleware:wrap-require-authentication private-routes) private-routes )
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
