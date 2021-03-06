This is just plumbing to use libsodium from Common Lisp.

# Installation

You need to install libsodium yourself, e. g. through your system's package
manager.  You also need the header files, which might come in a separate
package, e. g. libsodium-dev on Debian derivatives.

After installation, libsodium.so should be in /lib/, /usr/lib/, or
/usr/local/lib/, and a bunch of header files sodium/crypto_*.h somewhere the C
compiler finds them: the grovelling uses C preprocessor directives like
"#include <sodium/crypto_box.h>".


# Current status

It works on my computer (TM).  I am grateful for all feedback.

I have so far covered only authenticated encryption ("crypto_box") and scalar
multiplication ("crypto_scalarmult") including tests, more is to come.  Note
that the tests are only unit tests, thus test functionality, not that there are
no side channel attacks (in particular timing).

I am implementing only the default algorithm choices of NaCl (Curve25519,
XSalsa20, Poly1305) for now.


# History

I had translated TweetNaCl to Common Lisp at 30c3 (see
https://github.com/Harleqin/naclcl.git), but it was tedious trying to bring it
to work.  So, on 31c3, a new plan: first get it to work with CFFI, then I can
start to develop native (Lisp) drop-in replacements for parts of it.


# Documentation

All keys, nonces, messages, and ciphers are byte vectors, i. e. of type `(vector
(unsigned-byte 8))`.


## Authenticated encryption

Public-key authenticated encryption is done through a combination of Curve25519
keys, XSalsa20 stream cipher, and Poly1305 one-time authentication.

[function]  
`make-box-keypair` => public-key, secret-key

  Randomly generates a pair of corresponding public and secret key and returns
  them as two values, first public, then secret.  Guarantees that the secret key
  has `+box-secret-key-bytes+` bytes and the public key has
  `+box-public-key-bytes+` bytes.  Both these constants are grovelled from
  `sodium/crypto_box.h`.  Randomness is tapped from `/dev/urandom` through the
  libsodium `randombytes()` function.

[function]  
`box-message` message nonce receiver-public-key sender-secret-key => cipher

  Encrypts and authenticates message using the receiver's public key, the
  sender's secret key, and a nonce and returns the resulting ciphertext.
  Signals a type-error if sender-secret-key is not a `(vector (unsigned-byte 8)
  +box-secret-key-bytes+)` or if receiver-public-key is not a `(vector
  (unsigned-byte 8) +box-public-key-bytes+)` or if nonce is not a `(vector
  (unsigned-byte 8) +box-nonce-bytes+)` or if message is not a `(vector
  (unsigned-byte 8))`.

[function]  
`unbox-message` cipher nonce sender-public-key receiver-secret-key => message

  Verifies and decrypts cipher using the nonce, the sender's public key, and the
  receiver's secret key.  Returns the plaintext message.  Signals a
  box-verification-error if the ciphertext fails verification.  Signals a
  type-error if cipher is not a `(vector (unsigned-byte 8))` or if nonce is not a
  `(vector (unsigned-byte 8) +box-nonce-bytes+)` or if sender-public-key is not a
  `(vector (unsigned-byte 8) +box-public-key-bytes+)` or if receiver-secret-key is
  not a `(vector (unsigned-byte 8) +box-secret-key-bytes+)`.

[condition type]  
`box-verification-error` (error)

  This error signals a failed cipher verification.

[constant]  
`+box-nonce-bytes+`

  The exact length of a nonce byte vector.  For XSalsa20: 24.

[constant]  
`+box-public-key-bytes+`

  The exact length of a public key byte vector.  For Curve25519: 32.

[constant]  
`+box-secret-key-bytes+`

  The exact length of a secret key byte vector.  For Curve25519: 32.


## Scalar multiplication

Scalar multiplication on elliptic curves over finite fields is the basis for
generating keys and shared secrets in elliptic curve cryptography.  In
Curve25519, the secret key is a random 32 byte integer, and the public key a
base group element multiplied by that integer.  The shared secret of two
communicating parties is obtained through scalar multiplication of the public
key of any one of them by the secret key of the other.

[function]  
`scalarmult` n p => result

  Multiplies a group element p by an integer n and returns the result.  Signals
  a type-error if p is not a `(vector (unsigned-byte 8) +scalarmult-bytes+)` or n
  is not a `(vector (unsigned-byte 8) +scalarmult-scalarbytes+)`.

[function]  
`scalarmult-base` n => result

  Multiplies a base group element by an integer n and returns the result.
  Signals a type-error if n is not a `(vector (unsigned-byte 8)
  +scalarmult-scalarbytes+)`.

[constant]  
`+scalarmult-bytes+`

  The exact length of a group element byte vector. For Curve25519: 32.

[constant]  
`+scalarmult-scalarbytes+`

  The exact length of a multiplier byte vector for scalar multiplication.  For
  Curve25519: 32.


## Signatures

Signatures are provided through the Ed25519 curve.

[function]  
`make-sign-keypair` => public-key, secret-key

  Randomly generates a pair of corresponding public and secret key and returns
  them as two values, first public, then secret.  Guarantees that the secret key
  has `+sign-secret-key-bytes+` and the public key `+sign-public-key-bytes+`.

[function]  
`sign-message` message secret-key => signed-message

  Signs message with secret-key.  Returns the signed message.  Signals a
  type-error if message is not a `(vector (unsigned-byte 8))` or if secret-key is
  not a `(vector (unsigned-byte 8) +sign-secret-key-bytes+)`.

[function]  
`open-signed-message` signed-message public-key => message

  Verifies the signature in signed-message using public-key, then returns the
  plain message.  Signals a `signature-verification-error` if verification fails.
  Signals a `type-error` if signed-message is not a `(vector (unsigned-byte 8))` or
  if public-key is not a `(vector (unsigned-byte 8) +sign-public-key-bytes+)`.

[condition type]  
`signature-verification-error` (error)

  This error indicates a failed signature verification.

[constant]  
`+sign-public-key-bytes+`

  The exact length of a public key byte vector for signing.  For Ed25519: 32.

[constant]  
`+sign-secret-key-bytes+`

  The exact length of a secret key byte vector for signing.  For Ed25519: 64.
