(in-package #:foreign-sodium)

;;; Key pair generation

(defcfun (crypto-box-keypair "crypto_box_keypair") :int
  (pk (:pointer :unsigned-char))
  (sk (:pointer :unsigned-char)))

(defun make-box-keypair ()
  "Randomly generates a pair of corresponding public and secret key and returns
them as two values, first public, then secret.  Guarantees that the secret key
has +box-secret-key-bytes+ bytes and the public key has +box-public-key-bytes+
bytes.  Both these constants are grovelled from sodium/crypto_box.h.  Randomness
is tapped from /dev/urandom through the libsodium randombytes() function."
  (make-keypair #'crypto-box-keypair
                +box-public-key-bytes+
                +box-secret-key-bytes+))


;;; Encryption by Sender

(defcfun (crypto-box "crypto_box") :int
  (cipher (:pointer :unsigned-char))
  (message (:pointer :unsigned-char))
  (message-length :unsigned-long-long)
  (nonce (:pointer :unsigned-char))
  (receiver-public-key (:pointer :unsigned-char))
  (sender-secret-key (:pointer :unsigned-char)))

(defun box-message (message nonce receiver-public-key sender-secret-key
                    &aux
                      (length (+ (length message) +box-zerobytes+))
                      (padding (make-array +box-zerobytes+
                                           :element-type '(unsigned-byte 8))))
  "Encrypts and authenticates message using the receiver's public key, and the
sender's secret key and a nonce and returns the resulting ciphertext.  Signals a
type-error if sender-secret-key is not a \(vector \(unsigned-byte 8)
+box-secret-key-bytes+) or if receiver-public-key is not a \(vector
\(unsigned-byte 8) +box-public-key-bytes+) or if nonce is not a \(vector
\(unsigned-byte 8) +box-nonce-bytes+) or if message is not a \(vector
\(unsigned-byte 8))."
  (check-type message (vector (unsigned-byte 8)))
  (assert-type nonce `(vector (unsigned-byte 8) ,+box-nonce-bytes+))
  (assert-type receiver-public-key 
               `(vector (unsigned-byte 8) ,+box-public-key-bytes+))
  (assert-type sender-secret-key 
               `(vector (unsigned-byte 8) ,+box-secret-key-bytes+))
  (with-foreign-objects ((c-cipher :unsigned-char length)
                         (c-message :unsigned-char length)
                         (c-nonce :unsigned-char +box-nonce-bytes+)
                         (c-receiver-public-key :unsigned-char
                                                +box-public-key-bytes+)
                         (c-sender-secret-key :unsigned-char
                                              +box-secret-key-bytes+))
    (vector-to-c padding c-message +box-zerobytes+)
    (vector-to-c message
                 (inc-pointer c-message +box-zerobytes+)
                 (length message))
    (vector-to-c nonce c-nonce +box-nonce-bytes+ :unsigned-char)
    (vector-to-c receiver-public-key
                 c-receiver-public-key
                 +box-public-key-bytes+)
    (vector-to-c sender-secret-key
                 c-sender-secret-key
                 +box-secret-key-bytes+)
    (crypto-box c-cipher
                c-message
                length
                c-nonce
                c-receiver-public-key
                c-sender-secret-key)
    (vector-from-c (inc-pointer c-cipher +box-boxzerobytes+)
                   (- length +box-boxzerobytes+))))

;;; Decryption by Receiver

(define-condition box-verification-error (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Cipher could not be verified.")))
  (:documentation "This error signals a failed cipher verification."))

(defcfun (crypto-box-open "crypto_box_open") :int
  (message (:pointer :unsigned-char))
  (cipher (:pointer :unsigned-char))
  (cipher-length :unsigned-long-long)
  (nonce (:pointer :unsigned-char))
  (sender-public-key (:pointer :unsigned-char))
  (receiver-secret-key (:pointer :unsigned-char)))

(defun unbox-message (cipher nonce sender-public-key receiver-secret-key
                      &aux
                        (length (+ (length cipher) +box-boxzerobytes+))
                        (padding (make-array +box-boxzerobytes+
                                             :element-type '(unsigned-byte 8))))
  "Verifies and decrypts cipher using the nonce, the sender's public key, and
the receiver's secret key.  Returns the plaintext message.  Signals a
box-verification-error if the ciphertext fails verification.  Signals a
type-error if cipher is not a \(vector \(unsigned-byte 8)) or if nonce is not a
\(vector \(unsigned-byte 8) +box-nonce-bytes+) or if sender-public-key is not a
\(vector \(unsigned-byte 8) +box-public-key-bytes+) or if receiver-secret-key is
not a \(vector \(unsigned-byte 8) +box-secret-key-bytes+)."
  (check-type cipher (vector (unsigned-byte 8)))
  (assert-type nonce `(vector (unsigned-byte 8) ,+box-nonce-bytes+))
  (assert-type sender-public-key
               `(vector (unsigned-byte 8) ,+box-public-key-bytes+))
  (assert-type receiver-secret-key
               `(vector (unsigned-byte 8) ,+box-secret-key-bytes+))
  (with-foreign-objects ((c-message :unsigned-char length)
                         (c-cipher :unsigned-char length)
                         (c-nonce :unsigned-char +box-nonce-bytes+)
                         (c-sender-public-key :unsigned-char
                                              +box-public-key-bytes+)
                         (c-receiver-secret-key :unsigned-char
                                                +box-secret-key-bytes+))
    (vector-to-c padding c-cipher +box-boxzerobytes+)
    (vector-to-c cipher
                 (inc-pointer c-cipher +box-boxzerobytes+)
                 (length cipher))
    (vector-to-c nonce c-nonce +box-nonce-bytes+)
    (vector-to-c sender-public-key
                 c-sender-public-key
                 +box-public-key-bytes+)
    (vector-to-c receiver-secret-key
                 c-receiver-secret-key
                 +box-secret-key-bytes+)
    (let ((verifiedp (zerop (crypto-box-open c-message
                                             c-cipher
                                             length
                                             c-nonce
                                             c-sender-public-key
                                             c-receiver-secret-key))))
      (if verifiedp
          (vector-from-c (inc-pointer c-message +box-zerobytes+)
                         (- length +box-zerobytes+))
          (error 'box-verification-error)))))
