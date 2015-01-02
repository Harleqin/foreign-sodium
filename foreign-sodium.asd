(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system '#:cffi-grovel))

(defpackage #:foreign-sodium/asdf
  (:use #:cl
        #:asdf
        #:cffi-grovel))

(in-package #:foreign-sodium/asdf)

(defsystem #:foreign-sodium
  :author "Svante v. Erichsen <svante.v.erichsen@web.de>"
  :description "CFFI bindings for libsodium (http://doc.libsodium.org/), which
is a shared library fork of NaCl (https://nacl.cr.yp.to/)."
  :license "public domain"
  :serial t
  :depends-on (#:cffi
               #:hu.dwim.stefil)
  :components ((:file "packages")
               (grovel-file "grovel-libsodium-h")
               (:file "conversion")
               (:file "box")
               (:file "test-box")))