;;;; Accumulate varied signed integers and count positive values.

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load #.(merge-pathnames "perfutils/case.lisp"
                           (or *compile-file-truename* *load-truename*))
        :verbose nil :print nil))

(defparameter *name* "integer_accumulation")
(defparameter *description* "Signed integer accumulation with a data-dependent branch")
(defparameter *default-size* 1000000)
(defparameter *default-seed* 42)

(defun prepare (size seed)
  (declare (type fixnum size seed))
  (when (< size 0)
    (error "size must be nonnegative"))
  (let ((state (logand seed #xFFFFFFFF))
        (values (make-array size)))
    (declare (type (unsigned-byte 32) state)
             (type simple-vector values))
    (dotimes (i size values)
      (declare (type fixnum i))
      (setf state (logand (+ (* state 1664525) 1013904223) #xFFFFFFFF))
      (setf (svref values i) (- (ash state -8) #x800000)))))

(defun run-kernel (data)
  (declare (type simple-vector data))
  (let ((total 0)
        (positives 0)
        (n (length data)))
    (declare (type fixnum total positives n))
    (dotimes (i n)
      (declare (type fixnum i))
      (let ((value (the fixnum (svref data i))))
        (declare (type fixnum value))
        (incf total value)
        (when (> value 0)
          (incf positives))))
    (list total positives)))

(defun main ()
  (run-case *name* *description* *default-size* *default-seed* #'prepare #'run-kernel))

(eval-when (:execute)
  (main))
