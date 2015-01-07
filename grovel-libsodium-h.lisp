(in-package #:libsodium-h)

(include "sodium/crypto_box.h")
(include "sodium/crypto_scalarmult.h")
(include "sodium/crypto_sign.h")

;;; crypto_box.h

(constant (+box-public-key-bytes+ "crypto_box_PUBLICKEYBYTES")
          :documentation "The exact length of a public key byte vector.
For Curve25519: 32.")
(constant (+box-secret-key-bytes+ "crypto_box_SECRETKEYBYTES")
          :documentation "The exact length of a secret key byte vector.
For Curve25519: 32.")
(constant (+box-nonce-bytes+ "crypto_box_NONCEBYTES")
          :documentation "The exact length of a nonce byte vector.
For XSalsa20: 24.")
(constant (+box-zerobytes+ "crypto_box_ZEROBYTES")
          :documentation "The needed/resulting starting zero-padding of a
message.  For XSalsa20: 32.")
(constant (+box-boxzerobytes+ "crypto_box_BOXZEROBYTES")
          :documentation "The needed/resulting starting zero-padding of a
ciphertext.  For XSalsa20: 16.")

;;; crypto_scalarmult.h

(constant (+scalarmult-bytes+ "crypto_scalarmult_BYTES")
          :documentation "The exact length of a group element byte vector.
For Curve25519: 32.")

(constant (+scalarmult-scalarbytes+ "crypto_scalarmult_SCALARBYTES")
          :documentation "The exact length of a multiplier byte vector for
scalar multiplication.  For Curve25519: 32.")

;;; crypto_sign.h

(constant (+sign-public-key-bytes+ "crypto_sign_PUBLICKEYBYTES")
          :documentation "The exact length of a public key byte vector for
signing.  For Ed25519: 32.")

(constant (+sign-secret-key-bytes+ "crypto_sign_SECRETKEYBYTES")
          :documentation "The exact length of a secret key byte vector for
signing.  For Ed25519: 64.")

(constant (+sign-bytes+ "crypto_sign_BYTES")
          :documentation "The maximum length that a signed message takes more
than the unsigned message.")
