;;;; crypt.lisp
(in-package :cl-user)
(uiop:define-package :cl-natter.crypt
  (:use :cl)
  (:import-from #:ironclad
                #:constant-time-equal)
  (:export #:sha256
           #:sha256-string
           #:add-padding
           #:remove-padding
           #:base64-url-decode
           #:base64-url-encode
           #:base64-url-encode-string
           #:constant-time-equal))

(in-package :cl-natter.crypt)

(defvar *sha256-digest* (ironclad:make-digest :sha256))

(defun sha256 (buffer)
  (reinitialize-instance *sha256-digest*)
  (ironclad:update-digest *sha256-digest* buffer)
  (ironclad:produce-digest *sha256-digest*))

(defun sha256-string (string)
  (let ((buffer (ironclad:ascii-string-to-byte-array string)))
    (sha256 buffer)))

(defun add-padding (input)
  (let ((mod (mod (length input) 4)))
    (if (= mod 0)
        input
        (concatenate 'string input (make-string (- 4 mod) :initial-element #\.)))))

(defun remove-padding (input)
  (string-right-trim '(#\.) input))

(defun base64-url-decode (input)
  (base64:base64-string-to-usb8-array (add-padding input) :uri t))

(defun base64-url-encode (octets)
  (remove-padding (base64:usb8-array-to-base64-string octets :uri t)))

(defun base64-url-encode-string (string)
  (remove-padding (base64:string-to-base64-string string :uri t)))

(base64:base64-string-to-string "anJ1aXo6c2hvcnRwYXNzd29yZA.." :uri t)
