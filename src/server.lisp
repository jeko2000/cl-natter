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

(defvar *handler* nil
  "The application's clack handler.")

(defvar *default-http-server-port* 8001)

(defun stop-http-server ()
  (when *http-server*
    (rate-limiter:stop-rate-limiter)
    (clack:stop *http-server*)
    (setf *http-server* nil)))

(defun compute-handler ()
  (lambda (request)
    (funcall route:api-routes request)))

(defun start-http-server (&key (port *default-http-server-port*))
  (stop-http-server)
  (let ((permits-per-second 2))
    (rate-limiter:start-rate-limiter permits-per-second))
  (setf *handler* (compute-handler))
  (setf *http-server* (clack:clackup *handler* :port port)))
