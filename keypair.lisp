(in-package #:foreign-sodium)

(defun make-keypair (generator public-key-bytes secret-key-bytes)
  "Uses generator to generate a pair of corresponding public and secret key and
returns them as two values, first public, then secret.  Generator needs to be a
CFFI-wrapped C function that is designed to operate on two C buffers whose
length is given by public-key-bytes and secret-key-bytes."
  (with-foreign-objects ((public-key :unsigned-char public-key-bytes)
                         (secret-key :unsigned-char secret-key-bytes))
    (funcall generator public-key secret-key)
    (values (vector-from-c public-key public-key-bytes)
            (vector-from-c secret-key secret-key-bytes))))
