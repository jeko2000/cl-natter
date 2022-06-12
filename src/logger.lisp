;;;; logger.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.logger
  (:use :cl)
  (:shadow #:error #:warn #:debug)
  (:import-from :verbose)
  (:export #:debug
           #:info
           #:warn
           #:error
           #:initialize-logger))

(in-package :cl-natter.logger)

(defparameter *log-category* :cl-natter
  "The default log category when none is provided in the log.")

(defparameter *log-timestamp-format*
  (append
   local-time:+iso-8601-date-format+
   (list #\Space)
   '((:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:msec 3))))

(defmacro log-message (level datum &rest args)
  (if (symbolp datum)
      `(v:log ,level ,datum ,@args)
      `(v:log ,level *log-category* ,datum ,@args)))

(defmacro debug (datum &rest args)
  `(log-message :debug ,datum ,@args))

(defmacro info (datum &rest args)
  `(log-message :info ,datum ,@args))

(defmacro warn (datum &rest args)
  `(log-message :warn ,datum ,@args))

(defmacro error (datum &rest args)
  `(log-message :error ,datum ,@args))

(defun initialize-logger ()
  (setf v:*timestamp-format* *log-timestamp-format*)
  (info :logger "Successfully initialized logger"))
