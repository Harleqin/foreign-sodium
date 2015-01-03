(in-package #:foreign-sodium-test)

(defun u8vector (initial-contents)
  (map-into (make-array (length initial-contents)
                        :element-type '(unsigned-byte 8))
            #'identity
            initial-contents))

(defun random-vector (length)
  (map-into (make-array length
                        :element-type '(unsigned-byte 8))
            (lambda () (random 256))))

(defun make-nonce ()
  (random-vector foreign-sodium:+box-nonce-bytes+))

(defsuite* test-box)

;;; A roundtrip of key generation, encrypting, and decrypting.  This only tests
;;; that the encryption and decryption operations mirror each other, and that
;;; the generated keys can be used for them.
;;; Also covers the same area as test/box7.c in NaCl.
(deftest (roundtrip :in test-box) ()
  (multiple-value-bind (sender-public sender-secret)
      (foreign-sodium:make-box-keypair)
    (multiple-value-bind (receiver-public receiver-secret)
        (foreign-sodium:make-box-keypair)
      (let* ((message (map-into (make-array 100
                                            :element-type '(unsigned-byte 8))
                                (lambda () (random 256))))
             (nonce (make-nonce))
             (cipher (foreign-sodium:box-message message
                                                 nonce
                                                 receiver-public
                                                 sender-secret))
             (uncipher (foreign-sodium:unbox-message cipher
                                                     nonce
                                                     sender-public
                                                     receiver-secret)))
        (is (equalp uncipher message))))))

;;; The following are tests and example data from the original NaCl
;;; distribution.

;; test/box2.c
(deftest (test-box-encrypt-decrypt :in test-box) ()
  (let ((bob-secret (u8vector #(#x5d #xab #x08 #x7e #x62 #x4a #x8a #x4b
                                #x79 #xe1 #x7f #x8b #x83 #x80 #x0e #xe6
                                #x6f #x3b #xb1 #x29 #x26 #x18 #xb6 #xfd
                                #x1c #x2f #x8b #x27 #xff #x88 #xe0 #xeb)))
        (alice-public (u8vector #(#x85 #x20 #xf0 #x09 #x89 #x30 #xa7 #x54
                                  #x74 #x8b #x7d #xdc #xb4 #x3e #xf7 #x5a
                                  #x0d #xbf #x3a #x0d #x26 #x38 #x1a #xf4
                                  #xeb #xa4 #xa9 #x8e #xaa #x9b #x4e #x6a)))
        (nonce (u8vector #(#x69 #x69 #x6e #xe9 #x55 #xb6 #x2b #x73
                           #xcd #x62 #xbd #xa8 #x75 #xfc #x73 #xd6
                           #x82 #x19 #xe0 #x03 #x6b #x7a #x0b #x37)))
        ;; the cipher is zero-padded by our wrapper
        (cipher (u8vector #(#xf3 #xff #xc7 #x70 #x3f #x94 #x00 #xe5
                            #x2a #x7d #xfb #x4b #x3d #x33 #x05 #xd9
                            #x8e #x99 #x3b #x9f #x48 #x68 #x12 #x73
                            #xc2 #x96 #x50 #xba #x32 #xfc #x76 #xce
                            #x48 #x33 #x2e #xa7 #x16 #x4d #x96 #xa4
                            #x47 #x6f #xb8 #xc5 #x31 #xa1 #x18 #x6a
                            #xc0 #xdf #xc1 #x7c #x98 #xdc #xe8 #x7b
                            #x4d #xa7 #xf0 #x11 #xec #x48 #xc9 #x72
                            #x71 #xd2 #xc2 #x0f #x9b #x92 #x8f #xe2
                            #x27 #x0d #x6f #xb8 #x63 #xd5 #x17 #x38
                            #xb4 #x8e #xee #xe3 #x14 #xa7 #xcc #x8a
                            #xb9 #x32 #x16 #x45 #x48 #xe5 #x26 #xae
                            #x90 #x22 #x43 #x68 #x51 #x7a #xcf #xea
                            #xbd #x6b #xb3 #x73 #x2b #xc0 #xe9 #xda
                            #x99 #x83 #x2b #x61 #xca #x01 #xb6 #xde
                            #x56 #x24 #x4a #x9e #x88 #xd5 #xf9 #xb3
                            #x79 #x73 #xf6 #x22 #xa4 #x3d #x14 #xa6
                            #x59 #x9b #x1f #x65 #x4c #xb4 #x5a #x74
                            #xe3 #x55 #xa5)))
        (expected-message (u8vector #(#xbe #x07 #x5f #xc5 #x3c #x81 #xf2 #xd5
                                      #xcf #x14 #x13 #x16 #xeb #xeb #x0c #x7b
                                      #x52 #x28 #xc5 #x2a #x4c #x62 #xcb #xd4
                                      #x4b #x66 #x84 #x9b #x64 #x24 #x4f #xfc
                                      #xe5 #xec #xba #xaf #x33 #xbd #x75 #x1a
                                      #x1a #xc7 #x28 #xd4 #x5e #x6c #x61 #x29
                                      #x6c #xdc #x3c #x01 #x23 #x35 #x61 #xf4
                                      #x1d #xb6 #x6c #xce #x31 #x4a #xdb #x31
                                      #x0e #x3b #xe8 #x25 #x0c #x46 #xf0 #x6d
                                      #xce #xea #x3a #x7f #xa1 #x34 #x80 #x57
                                      #xe2 #xf6 #x55 #x6a #xd6 #xb1 #x31 #x8a
                                      #x02 #x4a #x83 #x8f #x21 #xaf #x1f #xde
                                      #x04 #x89 #x77 #xeb #x48 #xf5 #x9f #xfd
                                      #x49 #x24 #xca #x1c #x60 #x90 #x2e #x52
                                      #xf0 #xa0 #x89 #xbc #x76 #x89 #x70 #x40
                                      #xe0 #x82 #xf9 #x37 #x76 #x38 #x48 #x64
                                      #x5e #x07 #x05))))
    (is (equalp (foreign-sodium:unbox-message cipher
                                              nonce
                                              alice-public
                                              bob-secret)
                expected-message))))

;; test/box3.cpp, test/box.c
(deftest (test-box-encrypt :in test-box) ()
  (let ((alice-secret (u8vector #(#x77 #x07 #x6d #x0a #x73 #x18 #xa5 #x7d
                                  #x3c #x16 #xc1 #x72 #x51 #xb2 #x66 #x45
                                  #xdf #x4c #x2f #x87 #xeb #xc0 #x99 #x2a
                                  #xb1 #x77 #xfb #xa5 #x1d #xb9 #x2c #x2a)))
        (bob-public (u8vector #(#xde #x9e #xdb #x7d #x7b #x7d #xc1 #xb4
                                #xd3 #x5b #x61 #xc2 #xec #xe4 #x35 #x37
                                #x3f #x83 #x43 #xc8 #x5b #x78 #x67 #x4d
                                #xad #xfc #x7e #x14 #x6f #x88 #x2b #x4f)))
        (nonce (u8vector #(#x69 #x69 #x6e #xe9 #x55 #xb6 #x2b #x73
                           #xcd #x62 #xbd #xa8 #x75 #xfc #x73 #xd6
                           #x82 #x19 #xe0 #x03 #x6b #x7a #x0b #x37)))
        (message (u8vector #(#xbe #x07 #x5f #xc5 #x3c #x81 #xf2 #xd5
                             #xcf #x14 #x13 #x16 #xeb #xeb #x0c #x7b
                             #x52 #x28 #xc5 #x2a #x4c #x62 #xcb #xd4
                             #x4b #x66 #x84 #x9b #x64 #x24 #x4f #xfc
                             #xe5 #xec #xba #xaf #x33 #xbd #x75 #x1a
                             #x1a #xc7 #x28 #xd4 #x5e #x6c #x61 #x29
                             #x6c #xdc #x3c #x01 #x23 #x35 #x61 #xf4
                             #x1d #xb6 #x6c #xce #x31 #x4a #xdb #x31
                             #x0e #x3b #xe8 #x25 #x0c #x46 #xf0 #x6d
                             #xce #xea #x3a #x7f #xa1 #x34 #x80 #x57
                             #xe2 #xf6 #x55 #x6a #xd6 #xb1 #x31 #x8a
                             #x02 #x4a #x83 #x8f #x21 #xaf #x1f #xde
                             #x04 #x89 #x77 #xeb #x48 #xf5 #x9f #xfd
                             #x49 #x24 #xca #x1c #x60 #x90 #x2e #x52
                             #xf0 #xa0 #x89 #xbc #x76 #x89 #x70 #x40
                             #xe0 #x82 #xf9 #x37 #x76 #x38 #x48 #x64
                             #x5e #x07 #x05)))
        (expected-cipher (u8vector #(#xf3 #xff #xc7 #x70 #x3f #x94 #x00 #xe5
                                     #x2a #x7d #xfb #x4b #x3d #x33 #x05 #xd9
                                     #x8e #x99 #x3b #x9f #x48 #x68 #x12 #x73
                                     #xc2 #x96 #x50 #xba #x32 #xfc #x76 #xce
                                     #x48 #x33 #x2e #xa7 #x16 #x4d #x96 #xa4
                                     #x47 #x6f #xb8 #xc5 #x31 #xa1 #x18 #x6a
                                     #xc0 #xdf #xc1 #x7c #x98 #xdc #xe8 #x7b
                                     #x4d #xa7 #xf0 #x11 #xec #x48 #xc9 #x72
                                     #x71 #xd2 #xc2 #x0f #x9b #x92 #x8f #xe2
                                     #x27 #x0d #x6f #xb8 #x63 #xd5 #x17 #x38
                                     #xb4 #x8e #xee #xe3 #x14 #xa7 #xcc #x8a
                                     #xb9 #x32 #x16 #x45 #x48 #xe5 #x26 #xae
                                     #x90 #x22 #x43 #x68 #x51 #x7a #xcf #xea
                                     #xbd #x6b #xb3 #x73 #x2b #xc0 #xe9 #xda
                                     #x99 #x83 #x2b #x61 #xca #x01 #xb6 #xde
                                     #x56 #x24 #x4a #x9e #x88 #xd5 #xf9 #xb3
                                     #x79 #x73 #xf6 #x22 #xa4 #x3d #x14 #xa6
                                     #x59 #x9b #x1f #x65 #x4c #xb4 #x5a #x74
                                     #xe3 #x55 #xa5))))
    (is (equalp (foreign-sodium:box-message message
                                            nonce
                                            bob-public
                                            alice-secret)
                expected-cipher))))

;; test/box6.cpp, test/box8.c
(deftest (test-box-verification :in test-box) ()
  ;; Only some corner cases, so that the tests are still fast.
  (dolist (message-length '(0 1 2 7 8 9 15 16 17 31 32 33 63 64 65 127 128 129
                            255 256 257 511 512 513 767 768 769 1000))
    (multiple-value-bind (sender-public sender-secret)
        (foreign-sodium:make-box-keypair)
      (multiple-value-bind (receiver-public receiver-secret)
          (foreign-sodium:make-box-keypair)
        (let* ((nonce (make-nonce))
               (message (random-vector message-length))
               (cipher (foreign-sodium:box-message message
                                                   nonce
                                                   receiver-public
                                                   sender-secret)))
          (loop
            :with caught := 0
            :with unchanged := 0
            :while (< caught 10)
            :do (setf (aref cipher (random (length cipher)))
                      (random 256))
                (handler-case
                    (let ((changed-message
                           (foreign-sodium:unbox-message cipher
                                                         nonce
                                                         sender-public
                                                         receiver-secret)))
                      (is (equalp message changed-message)
                          "Forged message ~s from original ~s by changing the ~
                           cipher!"
                          changed-message
                          message)
                      (incf unchanged))
                  (foreign-sodium:box-verification-error ()
                    (incf caught))
                  (condition (c)
                    (is nil "Unexpected condition ~s!" c)))))))))
