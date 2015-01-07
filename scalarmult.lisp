(in-package #:foreign-sodium)

(defcfun (crypto-scalarmult "crypto_scalarmult") :int
  (q (:pointer :unsigned-char))
  (n (:pointer :unsigned-char))
  (p (:pointer :unsigned-char)))

(defun scalarmult (n p)
  "Multiplies a group element p by an integer n and returns the result.
Signals a type-error if p is not a \(vector \(unsigned-byte 8)
+scalarmult-bytes+) or n is not a \(vector \(unsigned-byte 8)
+scalarmult-scalarbytes+)."
  (assert-type n `(vector (unsigned-byte 8) ,+scalarmult-scalarbytes+))
  (assert-type p `(vector (unsigned-byte 8) ,+scalarmult-bytes+))
  (with-foreign-objects ((c-q :unsigned-char +scalarmult-bytes+)
                         (c-n :unsigned-char +scalarmult-scalarbytes+)
                         (c-p :unsigned-char +scalarmult-bytes+))
    (vector-to-c n c-n +scalarmult-scalarbytes+ :unsigned-char)
    (vector-to-c p c-p +scalarmult-bytes+ :unsigned-char)
    (crypto-scalarmult c-q c-n c-p)
    (vector-from-c c-q +scalarmult-bytes+ :unsigned-char '(unsigned-byte 8))))

(defcfun (crypto-scalarmult-base "crypto_scalarmult_base") :int
  (q (:pointer :unsigned-char))
  (n (:pointer :unsigned-char)))

(defun scalarmult-base (n)
  "Multiplies a base group element by an integer n and returns the result.
Signals a type-error if n is not a \(vector \(unsigned-byte 8)
+scalarmult-scalarbytes+)."
  (assert-type n `(vector (unsigned-byte 8) ,+scalarmult-scalarbytes+))
  (with-foreign-objects ((c-q :unsigned-char +scalarmult-bytes+)
                         (c-n :unsigned-char +scalarmult-scalarbytes+))
    (vector-to-c n c-n +scalarmult-scalarbytes+ :unsigned-char)
    (crypto-scalarmult-base c-q c-n)
    (vector-from-c c-q +scalarmult-bytes+ :unsigned-char '(unsigned-byte 8))))
