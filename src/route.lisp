;;;; route.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.route
  (:use :cl)
  (:local-nicknames (:space :cl-natter.controller.space)
                    (:middleware :cl-natter.middleware)
                    (:error :cl-natter.error))
  (:import-from :tiny-routes
                #:define-routes
                #:define-route
                #:define-post
                #:with-request
                #:pipe)
  (:export #:api-routes))

(in-package :cl-natter.route)

(defvar *owner-scanner*
  (ppcre:create-scanner "^[a-zA-Z][a-zA-Z0-9]{1,29}$"))

(define-routes space-routes
  (define-post "/spaces" (request)
    (with-request (json-body) request
      (let ((space-name (getf json-body :|name| ""))
            (owner (getf json-body :|owner| "")))
        (when (> (length space-name) 255)
          (error:natter-validation-error "space name too long"))
        (unless (ppcre:scan *owner-scanner* owner)
          (error:natter-validation-error "invalid username"))
        (let* ((space-id (space:create-space space-name owner))
               (uri (format nil "/spaces/~a" space-id)))
          (tiny:created uri (list :|name| space-name :|uri| uri)))))))

(define-routes api-routes
  (pipe space-routes
    (middleware:wrap-require-json-content-type)
    (middleware:wrap-condition)
    (middleware:wrap-rate-limiter)
    (middleware:wrap-response-json-body)
    ;; We should keep this middleware towards the end to prevent
    ;; spurious hunchentoot errors about closed response streams
    (middleware:wrap-request-json-body)
    (middleware:wrap-sane-headers))

  ;; catch all route
  (define-route ()
    (tiny:not-found "NOT_FOUND")))
