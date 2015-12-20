(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system '#:cffi-grovel))

(defpackage #:foreign-sodium/asdf
  (:use #:cl
        #:asdf
        #:cffi-grovel))

(in-package #:foreign-sodium/asdf)

(defsystem #:foreign-sodium
  :version "0.3.1"
  :author "Svante v. Erichsen <svante.v.erichsen@web.de>"
  :description "CFFI bindings for libsodium (http://doc.libsodium.org/), which
is a shared library fork of NaCl (https://nacl.cr.yp.to/).  Uses only the
original NaCl C API."
  :license "public domain"
  :serial t
  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:cffi
               #:hu.dwim.stefil)
  :components ((:file "packages")
               (grovel-file "grovel-libsodium-h")
               (:file "conversion")
               (:file "load")
               (:file "init")
               (:file "test")
               (:file "keypair")
               (:file "box")
               (:file "test-box")
               (:file "scalarmult")
               (:file "test-scalarmult")
               (:file "sign")
               (:file "test-sign")))
