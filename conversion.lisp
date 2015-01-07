(in-package #:foreign-sodium)

(defun vector-from-c (pointer size
                      &optional
                        (c-element-type :unsigned-char)
                        (lisp-element-type '(unsigned-byte 8)))
  (let ((vector (make-array size :element-type lisp-element-type)))
    (loop
      :for i :below size
      :do (setf (aref vector i)
                (mem-aref pointer c-element-type i)))
    vector))

(defun vector-to-c (vector pointer size
                    &optional (c-element-type :unsigned-char))
  (loop
    :for element :across vector
    :for i :below size
    :do (setf (mem-aref pointer c-element-type i) element)))

(defmacro assert-type (place type)
  "Replacement for check-type that does evaluate type (i. e. does not quote
it)."
  `(assert (typep ,place ,type)
           (,place)
           'type-error
           :datum ,place
           :expected-type ,type))
