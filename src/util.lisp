;;;; util.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.util
  (:use :cl)
  (:export #:error-response
           #:parse-basic-authorization
           #:map-plist-values))

(in-package :cl-natter.util)

(defun error-response (error-message)
  (declare (inline error-response))
  (list :|error| error-message))

(defun parse-basic-authorization (authorization)
  (when (uiop:string-prefix-p "Basic " (or authorization ""))
    (let* ((credentials (second (uiop:split-string authorization)))
           (credentials (base64:base64-string-to-string credentials)))
      (destructuring-bind (username password &rest other) (uiop:split-string credentials :separator '(#\:))
        (unless other
          (values username password))))))

(defun map-plist-values (plist function)
  (loop for (k v) on plist by #'cddr
        collect k
        collect (funcall function v)))
