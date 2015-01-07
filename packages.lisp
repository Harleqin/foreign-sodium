(in-package #:cl-user)

(defpackage #:libsodium-h
  (:use #:cl #:cffi-grovel)
  (:export #:+box-public-key-bytes+
           #:+box-secret-key-bytes+
           #:+box-nonce-bytes+
           #:+box-zerobytes+
           #:+box-boxzerobytes+
           #:+scalarmult-bytes+
           #:+scalarmult-scalarbytes+
           #:+sign-bytes+
           #:+sign-public-key-bytes+
           #:+sign-secret-key-bytes+))

(defpackage #:foreign-sodium
  (:use #:cl
        #:cffi
        #:libsodium-h)
  (:export #:+box-nonce-bytes+
           #:+box-public-key-bytes+
           #:+box-secret-key-bytes+
           #:+scalarmult-bytes+
           #:+scalarmult-scalarbytes+
           #:+sign-bytes
           #:+sign-public-key-bytes+
           #:+sign-secret-key-bytes+
           #:box-message
           #:box-verification-error
           #:make-box-keypair
           #:make-sign-keypair
           #:open-signed-message
           #:scalarmult
           #:scalarmult-base
           #:sign-message
           #:signature-verification-error
           #:unbox-message))

(defpackage #:foreign-sodium-test
  (:use #:cl
        #:hu.dwim.stefil))
