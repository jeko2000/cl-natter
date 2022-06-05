;;;; cl-natter.asd

(asdf:defsystem #:cl-natter
  :description "A Common-Lisp implementation of Neil Madden's Natter API."
  :author "Johnny Ruiz <johnny@ruiz-usa.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src/"
  :depends-on (:uiop
               :cl-dbi
               :tiny-routes
               :clack
               :jonathan
               :cl-ppcre
               :str
               :cl-bcrypt
               :cl-base64)
  :components ((:file "error")
               (:file "util")
               (:file "type")
               (:file "db")
               (:module "controller"
                :serial t
                :components ((:file "space")
                             (:file "user")
                             (:file "audit")
                             (:file "moderator")))
               (:file "rate-limiter")
               (:file "middleware")
               (:file "route")
               (:file "server")
               (:file "cl-natter"))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-natter-test))))
