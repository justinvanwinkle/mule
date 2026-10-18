;;;; Validate structured integer records using conditions for rejected inputs.

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load #.(merge-pathnames "perfutils/case.lisp"
                           (or *compile-file-truename* *load-truename*))
        :verbose nil :print nil))

(defparameter *name* "exception_validation")
(defparameter *description* "Nested validation raising and catching domain exceptions")
(defparameter *default-size* 50000)
(defparameter *default-seed* 42)

(define-condition validation-error (error)
  ((code :initarg :code :reader validation-error-code :type fixnum)))

(define-condition range-error (validation-error)
  ((penalty :initarg :penalty :reader range-error-penalty :type fixnum)))

(defun check-field (raw bound)
  (declare (type fixnum raw bound))
  (let ((mode (logand raw 7)))
    (declare (type fixnum mode))
    (when (= mode 0)
      (error 'validation-error :code (logand raw 63)))
    (when (= mode 5)
      (error 'range-error :code (logand raw 31) :penalty (logand (ash raw -4) 15)))
    (mod (the fixnum (+ (the fixnum (* raw 17)) 11)) bound)))

(defun validate-record (a b)
  (declare (type fixnum a b))
  (let ((first (check-field a 997))
        (second (check-field b 991)))
    (declare (type fixnum first second))
    (the fixnum (+ (the fixnum (* first 3)) second))))

(defun prepare (size seed)
  (declare (type fixnum size seed))
  (when (< size 0)
    (error "size must be nonnegative"))
  (let ((state (logand seed #x7FFFFFFF))
        (pairs (make-array size)))
    (declare (type fixnum state)
             (type simple-vector pairs))
    (dotimes (i size pairs)
      (declare (type fixnum i))
      (setf state (logand (+ (* state 1103515245) 12345) #x7FFFFFFF))
      (let ((a (logand (ash state -5) #x3FFF)))
        (setf state (logand (+ (* state 1103515245) 12345) #x7FFFFFFF))
        (let ((b (logand (ash state -5) #x3FFF)))
          (setf (svref pairs i) (cons a b)))))))

(defun run-kernel (data)
  (declare (type simple-vector data))
  (let* ((n (length data))
         (rounds (max 1 (floor n 225)))
         (valid-sum 0)
         (range-errors 0)
         (other-errors 0))
    (declare (type fixnum n rounds valid-sum range-errors other-errors))
    (dotimes (round-idx rounds)
      (declare (type fixnum round-idx))
      (let ((salt (logand round-idx 7)))
        (declare (type fixnum salt))
        (dotimes (i n)
          (declare (type fixnum i))
          (let* ((pair (svref data i))
                 (a (the fixnum (car pair)))
                 (b (the fixnum (cdr pair))))
            (declare (type fixnum a b))
            (handler-case
                (setf valid-sum
                      (logand (the fixnum
                                   (+ valid-sum
                                      (validate-record (logxor a salt) (the fixnum (+ b salt)))))
                              #x3FFFFFFF))
              (range-error (err)
                (incf range-errors)
                (setf valid-sum
                      (logand (the fixnum
                                   (+ valid-sum
                                      (the fixnum (validation-error-code err))
                                      (the fixnum (range-error-penalty err))))
                              #x3FFFFFFF)))
              (validation-error (err)
                (incf other-errors)
                (setf valid-sum
                      (logand (the fixnum
                                   (+ valid-sum (the fixnum (validation-error-code err))))
                              #x3FFFFFFF))))))))
    (list valid-sum range-errors other-errors)))

(defun main ()
  (run-case *name* *description* *default-size* *default-seed* #'prepare #'run-kernel))

(eval-when (:execute)
  (main))
