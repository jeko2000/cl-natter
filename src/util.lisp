;;;; util.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.util
  (:use :cl)
  (:export #:error-response))

(in-package :cl-natter.util)

(defun error-response (error-message)
  (declare (inline error-response))
  (list :|error| error-message))
