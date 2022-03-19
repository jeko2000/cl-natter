;;;; route.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.route
  (:use :cl)
  (:local-nicknames (:space :cl-natter.controller.space)
                    (:middleware :cl-natter.middleware))
  (:import-from :tiny-routes
                #:define-routes
                #:define-route
                #:define-post
                #:with-request
                #:pipe)
  (:export #:api-routes))

(in-package :cl-natter.route)

(define-routes space-routes
  (define-post "/spaces" (request)
    (with-request (json-body) request
      (let* ((space-name (getf json-body :|name|))
             (owner (getf json-body :|owner|))
             (space-id (space:create-space space-name owner))
             (uri (format nil "/spaces/~a" space-id)))
        (tiny:created uri (list :|name| space-name :|uri| uri))))))

(define-routes api-routes
  (pipe space-routes
    (middleware:wrap-request-json-body)
    (middleware:wrap-response-json-body))

  ;; catch all route
  (define-route ()
    (tiny:not-found "NOT_FOUND")))
