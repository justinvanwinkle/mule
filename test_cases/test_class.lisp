
(defclass Test ()
  ())
(defclass SubTest (Test)
  ())
(defmethod foo (self SubTest)
  (let ((x 5))
    (print x)))
(defun not_method ())
