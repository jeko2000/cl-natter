;;;; util.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.util
  (:use :cl)
  (:import-from :str)
  (:import-from :alexandria
                #:ensure-list)
  (:export #:error-response
           #:parse-basic-authorization
           #:map-plist-values
           #:to-string
           #:to-integer
           #:keywordize
           #:with-payload
           #:with-query-results
           #:with-request-payload
           #:with-path-parameters
           #:with-query-parameters))

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

(defun to-string (item)
  (format nil "~a" item))

(defun to-integer (item &optional default)
  (etypecase item
    (null default)
    (number item)
    (string (parse-integer item :junk-allowed t))))

(defun keywordize (name)
  (intern (string name) :keyword))

(defun name->indicator-list (name)
  (let ((name (string-downcase (string name)))
        (keywords '()))
    (push (keywordize name) keywords)
    (pushnew (keywordize (str:upcase name)) keywords)
    (pushnew (keywordize (str:camel-case name)) keywords)
    (pushnew (keywordize (str:snake-case name)) keywords)
    keywords))


(defmacro with-payload (payload-vars payload &body body)
  (let ((gpayload (gensym "payload")))
    `(let ((,gpayload ,payload))
       (let ,(mapcar (lambda (var)
                       (destructuring-bind (symbol &optional name) (ensure-list var)
                         `(,symbol (nth-value 1 (get-properties ,gpayload ',(name->indicator-list (or name symbol)))))))
              payload-vars)
         ,@body))))

(defmacro with-request-payload (payload-vars request &body body)
  (let ((grequest (gensym "request"))
        (gpayload (gensym "payload")))
    `(let* ((,grequest ,request)
            (,gpayload (getf ,grequest :json-body)))
       (with-payload ,payload-vars ,gpayload
         ,@body))))

(defmacro with-path-parameters (param-vars request &body body)
  (let ((grequest (gensym "request"))
        (gparams (gensym "path")))
    `(let* ((,grequest ,request)
            (,gparams (getf ,grequest :path-parameters)))
       (with-payload ,param-vars ,gparams
         ,@body))))

(defmacro with-query-parameters (query-vars request &body body)
  (let ((grequest (gensym "request"))
        (gparams (gensym "path")))
    `(let* ((,grequest ,request)
            (,gparams (getf ,grequest :query-parameters)))
       (with-payload ,query-vars ,gparams
         ,@body))))
