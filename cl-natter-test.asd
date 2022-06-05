;;;; cl-natter-test.asd

(asdf:defsystem #:cl-natter-test
  :description "A test system for cl-natter."
  :author "Johnny Ruiz <johnny@ruiz-usa.com>"
  :license  "MIT"
  :version "0.0.1"
  :pathname "t/"
  :depends-on (:parachute :cl-natter)
  :components ((:file "cl-natter-test"))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :cl-natter-test)))
