(in-package #:foreign-sodium-test)

(defsuite* (test-sign :in test-foreign-sodium))

(defparameter *message*
  (let ((message "Hello, world!"))
    (map-into (make-array (length message)
                          :element-type '(unsigned-byte 8))
              #'char-code
              message)))

(deftest (test-make-sign-keypair :in test-sign) ()
  (multiple-value-bind (public secret) (foreign-sodium:make-sign-keypair)
    (is (typep public `(vector (unsigned-byte 8)
                               ,foreign-sodium:+sign-public-key-bytes+)))
    (is (typep secret `(vector (unsigned-byte 8)
                               ,foreign-sodium:+sign-secret-key-bytes+)))))

(deftest (test-sign-roundtrip :in test-sign) ()
  (multiple-value-bind (public secret) (foreign-sodium:make-sign-keypair)
    (let ((signed (foreign-sodium:sign-message *message* secret)))
      (is (equalp (foreign-sodium:open-signed-message signed public)
                  *message*)))))

(deftest (test-sign-verification-failure :in test-sign) ()
  (multiple-value-bind (public secret) (foreign-sodium:make-sign-keypair)
    (let ((signed (foreign-sodium:sign-message *message* secret)))
      (dotimes (i (length signed))
        (let ((manipulated (copy-seq signed)))
          (if (< (aref manipulated i) 128)
              (incf (aref manipulated i))
              (decf (aref manipulated i)))
          (signals foreign-sodium:signature-verification-error
            (foreign-sodium:open-signed-message manipulated public)))))))
