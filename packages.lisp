(in-package #:cl-user)

(defpackage #:libsodium-h
  (:use #:cffi-grovel)
  (:export #:+box-public-key-bytes+
           #:+box-secret-key-bytes+
           #:+box-nonce-bytes+
           #:+box-zerobytes+
           #:+box-boxzerobytes+))

(defpackage #:foreign-sodium
  (:use #:cl
        #:cffi
        #:libsodium-h)
  (:export #:+box-nonce-bytes+
           #:box-message
           #:box-verification-error
           #:make-box-keypair
           #:unbox-message))

(defpackage #:foreign-sodium-test
  (:use #:cl
        #:hu.dwim.stefil))
