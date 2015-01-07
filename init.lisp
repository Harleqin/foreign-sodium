(in-package #:foreign-sodium)

;;; Necessary for thread safety, according to crypto_box.h

(defcfun (sodium-init "sodium_init") :void)

(sodium-init)
