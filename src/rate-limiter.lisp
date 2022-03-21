;;;; rate-limiter.lisp

;;; This is a naive implementation of a rate limiter whereby we use a)
;;; a semaphore to keep track of the available permits, and b) a timer
;;; to periodically drain & replenish the semaphore with the right
;;; number of permits.
(in-package :cl-user)
(uiop:define-package :cl-natter.rate-limiter
  (:use :cl)
  (:export #:try-acquire
           #:permits
           #:stop-rate-limiter
           #:start-rate-limiter))

(in-package :cl-natter.rate-limiter)

(defvar *semaphore* nil)

(defvar *timer* nil)

(defun try-acquire (&optional (permits 1))
  "Acquire PERMITS from the rate limiter if available. Otherwise,
return `NIL'."
  (sb-thread:try-semaphore *semaphore* permits))

(defun stop-rate-limiter ()
  (setf *semaphore* nil)
  (when *timer*
    (sb-ext:unschedule-timer *timer*)
    (setf *timer* nil)))

(defun start-rate-limiter (permits-per-second)
  (check-type permits-per-second (integer 1))
  (flet ((replenish-semaphore ()
           ;; drain semaphore before replenishing
           (loop for attempt = (sb-thread:try-semaphore *semaphore* 1)
                 unless attempt do (return))
           (sb-thread:signal-semaphore *semaphore* permits-per-second)))
    (setf *semaphore* (sb-thread:make-semaphore :name "RATE_LIMITER_SEMAPHORE" :count 0)
          *timer* (sb-ext:make-timer #'replenish-semaphore :name "RATE_LIMITER_TIMER" :thread t))
    ;; schedule timer to run every second
    (sb-ext:schedule-timer *timer* 1 :repeat-interval 1)))
