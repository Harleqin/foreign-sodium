(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system '#:cffi-grovel))

(defpackage #:foreign-sodium/asdf
  (:use #:cl
        #:asdf
        #:cffi-grovel))

(in-package #:foreign-sodium/asdf)

(defsystem #:foreign-sodium
  :version "0.3.2"
  :author "Svante v. Erichsen <svante.v.erichsen@web.de>"
  :description "CFFI bindings for libsodium (http://doc.libsodium.org/), which
is a shared library fork of NaCl (https://nacl.cr.yp.to/).  Uses only the
original NaCl C API."
  :license "public domain"
  :serial t
  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:cffi)
  :components ((:file "packages")
               (grovel-file "grovel-libsodium-h")
               (:file "conversion")
               (:file "load")
               (:file "init")
               (:file "keypair")
               (:file "box")
               (:file "scalarmult")
               (:file "sign"))
  :in-order-to ((test-op (test-op #:foreign-sodium-test))))

(defsystem #:foreign-sodium-test
  :version "0.3.2"
  :author "Svante v. Erichsen <svante.v.erichsen@web.de>"
  :description "Tests for foreign-sodium"
  :license "public domain"
  :serial t
  :depends-on (#:hu.dwim.stefil)
  :components ((:module "test"
                        :serial t
                        :components ((:file "package")
                                     (:file "test-suite")
                                     (:file "box-test")
                                     (:file "scalarmult-test")
                                     (:file "sign-test"))))
  :perform (test-op (o c) (uiop:symbol-call 'foreign-sodium-test
                                            'test-foreign-sodium)))
