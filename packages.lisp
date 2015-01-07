(in-package #:cl-user)

(defpackage #:libsodium-h
  (:use #:cl #:cffi-grovel)
  (:export #:+box-public-key-bytes+
           #:+box-secret-key-bytes+
           #:+box-nonce-bytes+
           #:+box-zerobytes+
           #:+box-boxzerobytes+
           #:+scalarmult-bytes+
           #:+scalarmult-scalarbytes+))

(defpackage #:foreign-sodium
  (:use #:cl
        #:cffi
        #:libsodium-h)
  (:export #:+box-nonce-bytes+
           #:+box-public-key-bytes+
           #:+box-secret-key-bytes+
           #:box-message
           #:box-verification-error
           #:make-box-keypair
           #:scalarmult
           #:scalarmult-base
           #:unbox-message))

(defpackage #:foreign-sodium-test
  (:use #:cl
        #:hu.dwim.stefil))
