(in-package #:foreign-sodium)

(define-foreign-library libsodium
  (t "libsodium.so"))

(defun load-libsodium ()
  (let ((cffi:*foreign-library-directories* (list #p"/lib/"
                                                  #p"/usr/lib/"
                                                  #p"/usr/local/lib/")))
    (use-foreign-library libsodium)))

(load-libsodium)

;;; Necessary for thread safety, according to crypto_box.h
(defcfun (sodium-init "sodium_init") :void)
(sodium-init)

;;; Key pair generation

(defcfun (crypto-box-keypair "crypto_box_keypair") :int
  (pk (:pointer :unsigned-char))
  (sk (:pointer :unsigned-char)))

(defun make-box-keypair ()
  "Creates a pair of public and secret key and returns them as two values."
  (with-foreign-objects ((public-key :unsigned-char +box-public-key-bytes+)
                         (secret-key :unsigned-char +box-secret-key-bytes+))
    (crypto-box-keypair public-key secret-key)
    (values (vector-from-c public-key
                         +box-public-key-bytes+
                         :unsigned-char
                         '(unsigned-byte 8))
            (vector-from-c secret-key
                         +box-secret-key-bytes+
                         :unsigned-char
                         '(unsigned-byte 8)))))


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
  "Encrypts a message with a nonce, the receiver's public key, and the sender's
secret key."
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
    (vector-to-c padding c-message +box-zerobytes+ :unsigned-char)
    (vector-to-c message
                 (inc-pointer c-message +box-zerobytes+)
                 (length message)
                 :unsigned-char)
    (vector-to-c nonce c-nonce +box-nonce-bytes+ :unsigned-char)
    (vector-to-c receiver-public-key
                 c-receiver-public-key
                 +box-public-key-bytes+
                 :unsigned-char)
    (vector-to-c sender-secret-key
                 c-sender-secret-key
                 +box-secret-key-bytes+
                 :unsigned-char)
    (crypto-box c-cipher
                c-message
                length
                c-nonce
                c-receiver-public-key
                c-sender-secret-key)
    (vector-from-c (inc-pointer c-cipher +box-boxzerobytes+)
                   (- length +box-boxzerobytes+)
                   :unsigned-char
                   '(unsigned-byte 8))))

;;; Decryption by Receiver

(define-condition box-verification-error (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Cipher could not be verified."))))

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
    (vector-to-c padding c-cipher +box-boxzerobytes+ :unsigned-char)
    (vector-to-c cipher
                 (inc-pointer c-cipher +box-boxzerobytes+)
                 (length cipher)
                 :unsigned-char)
    (vector-to-c nonce c-nonce +box-nonce-bytes+ :unsigned-char)
    (vector-to-c sender-public-key
                 c-sender-public-key
                 +box-public-key-bytes+
                 :unsigned-char)
    (vector-to-c receiver-secret-key
                 c-receiver-secret-key
                 +box-secret-key-bytes+
                 :unsigned-char)
    (let* ((int (crypto-box-open c-message
                                 c-cipher
                                 length
                                 c-nonce
                                 c-sender-public-key
                                 c-receiver-secret-key))
           (verifiedp (zerop int)))
      (when (not verifiedp)
        (error 'box-verification-error))
      (vector-from-c (inc-pointer c-message +box-zerobytes+)
                     (- length +box-zerobytes+)
                     :unsigned-char
                     '(unsigned-byte 8)))))
