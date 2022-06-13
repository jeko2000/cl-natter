;;;; cl-natter.asd

(asdf:defsystem #:cl-natter
  :description "A Common-Lisp implementation of Neil Madden's Natter API."
  :author "Johnny Ruiz <johnny@ruiz-usa.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src/"
  :depends-on (:cl-base64
               :cl-bcrypt
               :cl-cookie
               :cl-dbi
               :cl-ppcre
               :clack
               :jonathan
               :str
               :synchronized-hash-tables
               :tiny-routes
               :tiny-routes-middleware-cookie
               :uuid
               :uiop
               :verbose)
  :components ((:file "error")
               (:file "logger")
               (:file "util")
               (:file "type")
               (:file "db")
               (:file "session")
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
