(in-package #:foreign-sodium)

(define-foreign-library libsodium
  (t "libsodium.so"))

(use-foreign-library libsodium)
