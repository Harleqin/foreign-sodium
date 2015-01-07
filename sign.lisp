(in-package #:foreign-sodium)

(defcfun (crypto-sign-keypair "crypto_sign_keypair") :int
  (pk (:pointer :unsigned-char))
  (sk (:pointer :unsigned-char)))

(defun make-sign-keypair ()
  "Randomly generates a pair of corresponding public and secret key and returns
them as two values, first public, then secret.  Guarantees that the secret key
has +sign-secret-key-bytes+ and the public key +sign-public-key-bytes+."
  (make-keypair #'crypto-sign-keypair
                +sign-public-key-bytes+
                +sign-secret-key-bytes+))

(defcfun (crypto-sign "crypto_sign") :int
  (signed-message (:pointer :unsigned-char))
  (signed-length (:pointer :unsigned-long-long))
  (message (:pointer :unsigned-char))
  (message-length :unsigned-long-long)
  (secret-key (:pointer :unsigned-char)))

(defun sign-message (message secret-key
                     &aux
                       (message-length (length message))
                       (signed-length (+ message-length +sign-bytes+)))
  "Signs message with secret-key.  Returns the signed message.  Signals a
type-error if message is not a \(vector \(unsigned-byte 8)) or if secret-key is
not a \(vector \(unsigned-byte 8) +sign-secret-key-bytes+)."
  (check-type message (vector (unsigned-byte 8)))
  (assert-type secret-key `(vector (unsigned-byte 8) ,+sign-secret-key-bytes+))
  (with-foreign-objects ((c-signed-message :unsigned-char signed-length)
                         (c-signed-length :unsigned-long-long)
                         (c-message :unsigned-char message-length)
                         (c-secret-key :unsigned-char +sign-secret-key-bytes+))
    (vector-to-c message c-message message-length)
    (vector-to-c secret-key c-secret-key +sign-secret-key-bytes+)
    (crypto-sign c-signed-message
                 c-signed-length
                 c-message
                 message-length
                 c-secret-key)
    (vector-from-c c-signed-message signed-length)))

(define-condition signature-verification-error (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Signature could not be verified.")))
  (:documentation "This error signals a failed signature verification."))

(defcfun (crypto-sign-open "crypto_sign_open") :int
  (message (:pointer :unsigned-char))
  (message-length (:pointer :unsigned-long-long))
  (signed-message (:pointer :unsigned-char))
  (signed-length :unsigned-long-long)
  (public-key (:pointer :unsigned-char)))

(defun open-signed-message (signed-message public-key
                            &aux
                              (signed-length (length signed-message)))
  "Verifies the signature in signed-message using public-key, then returns the
plain message.  Signals a signature-verification-error if verification fails.
Signals a type-error if signed-message is not a \(vector \(unsigned-byte 8)) or
if public-key is not a \(vector \(unsigned-byte 8) +sign-public-key-bytes+)."
  (check-type signed-message (vector (unsigned-byte 8)))
  (assert-type public-key `(vector (unsigned-byte 8) ,+sign-public-key-bytes+))
  (with-foreign-objects ((c-message :unsigned-char signed-length)
                         (c-message-length :unsigned-long-long)
                         (c-signed-message :unsigned-char signed-length)
                         (c-public-key :unsigned-char +sign-public-key-bytes+))
    (vector-to-c signed-message c-signed-message signed-length)
    (vector-to-c public-key c-public-key +sign-public-key-bytes+)
    (let* ((successp (zerop (crypto-sign-open c-message
                                              c-message-length
                                              c-signed-message
                                              signed-length
                                              c-public-key)))
           ;; The C API seems to be written under the assumption that the exact
           ;; message length is not knowable beforehand, except that it is at
           ;; most as long as the signed-message.
           (message-length (mem-ref c-message-length :unsigned-long-long)))
      (if successp
          (vector-from-c c-message message-length)
          (error 'signature-verification-error)))))
