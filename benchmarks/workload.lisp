;;; Native Common Lisp port of workload.py. Inputs and algorithms are identical.
(defpackage #:mule-benchmark
  (:use #:cl)
  (:shadow #:gcd)
  (:export #:main))

(in-package #:mule-benchmark)

(declaim (optimize (speed 3) (safety 1) (debug 0)))
(declaim (ftype (function ((unsigned-byte 15) (unsigned-byte 15)
                          (unsigned-byte 15) (unsigned-byte 15))
                         (unsigned-byte 31))
                queens))

(defun queens (mask columns left right)
  (declare (type (unsigned-byte 15) mask columns left right))
  (if (= columns mask)
      1
      (let ((available (logand mask (lognot (logior columns left right))))
            (total 0))
        (declare (type (unsigned-byte 15) available)
                 (type (unsigned-byte 31) total))
        (loop while (not (zerop available)) do
          (let ((bit (logand available (- available))))
            (declare (type (unsigned-byte 15) bit))
            (setf available (logxor available bit))
            (incf total
                  (queens mask
                          (logior columns bit)
                          (logand (ash (logior left bit) 1) mask)
                          (ash (logior right bit) -1)))))
        total)))

(declaim (ftype (function ((unsigned-byte 17) (unsigned-byte 17))
                         (unsigned-byte 17))
                gcd))

(defun gcd (a b)
  (declare (type (unsigned-byte 17) a b))
  (loop while (not (zerop b)) do
    (let ((remainder (mod a b)))
      (setf a b b remainder)))
  a)

(defun is-prime (n)
  (declare (type (integer 0 99999) n))
  (cond ((< n 2) nil)
        ((zerop (mod n 2)) (= n 2))
        ((zerop (mod n 3)) (= n 3))
        (t
         (let ((divisor 5))
           (declare (type (integer 5 325) divisor))
           (loop while (<= (* divisor divisor) n) do
             (when (or (zerop (mod n divisor))
                       (zerop (mod n (+ divisor 2))))
               (return-from is-prime nil))
             (incf divisor 6))
           t))))

(defun integer-kernel (iterations)
  ;; The loop count remains an arbitrary nonnegative integer, like Python's.
  ;; Only the bounded state and checksum arithmetic require machine integers.
  (declare (type (integer 0 *) iterations))
  (let ((state 123456789)
        (checksum 0)
        (index 0))
    (declare (type (unsigned-byte 31) state)
             (type (integer 0 1000000006) checksum)
             (type (integer 0 *) index))
    (loop while (< index iterations) do
      (setf state (logand (+ (* state 1664525) 1013904223) 2147483647))
      (let* ((candidate (+ 10000 (mod state 90000)))
             (common (gcd candidate (+ (mod (ash state -8) 10000) 1))))
        (declare (type (integer 10000 99999) candidate)
                 (type (unsigned-byte 17) common))
        (setf checksum
              (if (is-prime candidate)
                  (mod (+ (* checksum 33) candidate common) 1000000007)
                  (mod (+ (* checksum 33) (logxor candidate common)) 1000000007))))
      (incf index))
    checksum))

(defun main ()
  (declare (optimize (speed 1) (safety 3)))
  (let ((n (parse-integer (read-line)))
        (repetitions (parse-integer (read-line)))
        (iterations (parse-integer (read-line))))
    (when (or (< n 1) (> n 15) (< repetitions 0) (< iterations 0))
      (write-line "invalid parameters")
      (return-from main (values)))
    (let ((mask (1- (ash 1 (the (integer 1 15) n))))
          (solutions 0)
          (repeat 0))
      (declare (type (unsigned-byte 15) mask)
               (type (integer 0 *) solutions repeat))
      (loop while (< repeat repetitions) do
        (incf solutions (queens mask 0 0 0))
        (incf repeat))
      (format t "~D ~D~%" solutions (integer-kernel iterations))))
  (finish-output)
  (values))
