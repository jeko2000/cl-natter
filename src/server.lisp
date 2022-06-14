;;;; server.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.server
  (:use :cl)
  (:local-nicknames (:route :cl-natter.route)
                    (:rate-limiter :cl-natter.rate-limiter))
  (:export #:*default-http-server-port*
           #:start-http-server
           #:stop-http-server))

(in-package :cl-natter.server)

(defvar *http-server* nil
  "The application's HTTP server.")

(defvar *default-http-server-port* 8001)

(defun stop-http-server ()
  (when *http-server*
    (rate-limiter:stop-rate-limiter)
    (clack:stop *http-server*)
    (setf *http-server* nil)))

(defun compute-handler ()
  (lambda (request)
    (funcall route:app-routes request)))

(defun compute-ssl-parameters ()
  (let ((key (asdf:system-relative-pathname :cl-natter "ssl/localhost-key.pem"))
        (cert (asdf:system-relative-pathname :cl-natter "ssl/localhost.pem")))
    (when (and (uiop:file-exists-p key)
               (uiop:file-exists-p cert))
      (values t key cert))))

(defun start-http-server (&key (port *default-http-server-port*))
  (stop-http-server)
  (let ((permits-per-second 2))
    (rate-limiter:start-rate-limiter permits-per-second))
  (multiple-value-bind (ssl key-file cert-file) (compute-ssl-parameters)
    (setf *http-server*
          (clack:clackup
           (compute-handler)
           :port port
           :ssl ssl
           :ssl-key-file key-file
           :ssl-cert-file cert-file))))
