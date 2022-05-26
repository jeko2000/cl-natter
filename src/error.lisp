;;;; error.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.error
  (:use :cl)
  (:export #:natter-error
           #:natter-client-error
           #:natter-server-error
           #:natter-validation-error
           #:natter-not-found-error))

(in-package :cl-natter.error)

(define-condition natter-error (simple-error) ()
  (:documentation "The top-level application error."))

(define-condition natter-client-error (natter-error) ())

(define-condition natter-server-error (natter-error) ())

(define-condition natter-validation-error (natter-client-error) ())

(define-condition natter-not-found-error (natter-client-error) ())

(defun natter-error (class format-control &rest format-arguments)
  (error class
         :format-control format-control
         :format-arguments format-arguments))

(defun natter-validation-error (format-control &rest format-arguments)
  (apply #'natter-error 'natter-validation-error format-control format-arguments))

(defun natter-not-found-error (format-control &rest format-arguments)
  (apply #'natter-error 'natter-not-found-error format-control format-arguments))
