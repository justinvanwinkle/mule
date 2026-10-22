;;;; Accumulate varied signed integers with signed-byte 64 declarations and speed=3, safety=0.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load #.(merge-pathnames "perfutils/case.lisp"
                           (or *compile-file-truename* *load-truename*))
        :verbose nil :print nil))

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defparameter *name* "integer_accumulation_typed")
(defparameter *description* "Signed integer accumulation with int64 annotations and speed=3, safety=0")
(defparameter *default-size* 1000000)
(defparameter *default-seed* 42)

(defun prepare (size seed)
  (declare (type (signed-byte 64) size seed))
  (when (< size 0)
    (error "size must be nonnegative"))
  (let ((state (logand seed #xFFFFFFFF))
        (values (make-array size :element-type '(signed-byte 64))))
    (declare (type (unsigned-byte 32) state)
             (type (simple-array (signed-byte 64) (*)) values))
    (dotimes (i size values)
      (declare (type (signed-byte 64) i))
      (setf state (logand (+ (* state 1664525) 1013904223) #xFFFFFFFF))
      (setf (aref values i) (- (ash state -8) #x800000)))))

(defun run-kernel (data)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array (signed-byte 64) (*)) data))
  (let ((total 0)
        (positives 0)
        (n (length data)))
    (declare (type (signed-byte 64) total positives n))
    (dotimes (i n)
      (declare (type (signed-byte 64) i))
      (let ((value (aref data i)))
        (declare (type (signed-byte 64) value))
        (incf total value)
        (when (> value 0)
          (incf positives))))
    (list total positives)))

(defun main ()
  (run-case *name* *description* *default-size* *default-seed* #'prepare #'run-kernel))

(eval-when (:execute)
  (main))
