;;;; Stream integer signals through chained stateful generators and run-length coalescing.

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load #.(merge-pathnames "perfutils/case.lisp"
                           (or *compile-file-truename* *load-truename*))
        :verbose nil :print nil))

(defparameter *name* "generator_pipeline")
(defparameter *description* "Chained yield generators with sliding-window deltas and run-length coalescing")
(defparameter *default-size* 75000)
(defparameter *default-seed* 42)

(defun prepare (size seed)
  (declare (type fixnum size seed))
  (when (< size 1)
    (error "size must be positive"))
  (let ((state (logand seed #x7FFFFFFF))
        (values (make-array size)))
    (declare (type fixnum state)
             (type simple-vector values))
    (dotimes (i size values)
      (declare (type fixnum i))
      (setf state (logand (+ (* state 1103515245) 12345) #x7FFFFFFF))
      (setf (svref values i) (- (mod (ash state -8) 2001) 1000)))))

(defun make-filtered-gen (values limit bias)
  (declare (type simple-vector values)
           (type fixnum limit bias))
  (let ((n (length values))
        (idx 0)
        (has-pending nil)
        (pending-idx 0)
        (pending-val 0))
    (declare (type fixnum n idx pending-idx pending-val))
    (lambda ()
      (if has-pending
          (progn
            (setf has-pending nil)
            (values t pending-idx pending-val))
          (loop while (< idx n) do
            (let* ((cur-idx idx)
                   (raw (the fixnum (svref values cur-idx)))
                   (val (the fixnum (+ raw bias))))
              (declare (type fixnum cur-idx raw val))
              (incf idx)
              (when (and (<= (- limit) val) (<= val limit))
                (when (zerop (mod val 7))
                  (setf has-pending t
                        pending-idx cur-idx
                        pending-val (floor val 3)))
                (return (values t cur-idx val))))
            finally (return (values nil 0 0)))))))

(defun make-deltas-gen (upstream)
  (declare (type function upstream))
  (let ((have-prev1 nil)
        (have-prev2 nil)
        (prev1 0)
        (prev2 0))
    (declare (type fixnum prev1 prev2))
    (lambda ()
      (multiple-value-bind (ok idx val) (funcall upstream)
        (declare (type fixnum idx val))
        (if (not ok)
            (values nil 0 0 0 0)
            (let* ((d1 (if have-prev1 (the fixnum (- val prev1)) val))
                   (d2 (if have-prev2
                           (the fixnum (+ (the fixnum (- val (the fixnum (* 2 prev1)))) prev2))
                           d1)))
              (declare (type fixnum d1 d2))
              (setf prev2 prev1
                    prev1 val
                    have-prev2 have-prev1
                    have-prev1 t)
              (values t idx val d1 d2)))))))

(defun make-runs-gen (delta-stream)
  (declare (type function delta-stream))
  (let ((cur-sign 0)
        (run-len 0)
        (run-sum 0)
        (max-curv 0)
        (exhausted nil))
    (declare (type fixnum cur-sign run-len run-sum max-curv))
    (lambda ()
      (if exhausted
          (values nil 0 0 0 0)
          (loop
            (multiple-value-bind (ok idx val d1 d2) (funcall delta-stream)
              (declare (ignore idx)
                       (type fixnum val d1 d2))
              (if (not ok)
                  (progn
                    (setf exhausted t)
                    (if (> run-len 0)
                        (let ((s cur-sign) (l run-len) (r run-sum) (m max-curv))
                          (setf run-len 0)
                          (return (values t s l r m)))
                        (return (values nil 0 0 0 0))))
                  (let ((sign (cond ((> d1 0) 1) ((< d1 0) -1) (t 0)))
                        (curv (if (>= d2 0) d2 (the fixnum (- d2)))))
                    (declare (type fixnum sign curv))
                    (cond
                      ((= run-len 0)
                       (setf cur-sign sign
                             run-len 1
                             run-sum val
                             max-curv curv))
                      ((and (= sign cur-sign) (< run-len 16))
                       (incf run-len)
                       (incf run-sum val)
                       (when (> curv max-curv)
                         (setf max-curv curv)))
                      (t
                       (let ((out-sign cur-sign)
                             (out-len run-len)
                             (out-sum run-sum)
                             (out-curv max-curv))
                         (setf cur-sign sign
                               run-len 1
                               run-sum val
                               max-curv curv)
                         (return (values t out-sign out-len out-sum out-curv)))))))))))))

(defun run-kernel (data)
  (declare (type simple-vector data))
  (let ((run-count 0)
        (total-len 0)
        (weighted-sum 0)
        (curv-digest 0))
    (declare (type fixnum run-count total-len weighted-sum curv-digest))
    (dotimes (round-idx 4)
      (declare (type fixnum round-idx))
      (let* ((limit (+ 750 (* round-idx 60)))
             (bias (* (- round-idx 1) 15))
             (pipeline (make-runs-gen
                        (make-deltas-gen
                         (make-filtered-gen data limit bias)))))
        (declare (type fixnum limit bias)
                 (type function pipeline))
        (loop
          (multiple-value-bind (ok sign length rsum mcurv) (funcall pipeline)
            (declare (type fixnum sign length rsum mcurv))
            (unless ok
              (return))
            (incf run-count)
            (incf total-len length)
            (incf weighted-sum (the fixnum (+ (the fixnum (* sign rsum))
                                              (the fixnum (* length 5)))))
            (setf curv-digest
                  (logand (+ (* curv-digest 31) mcurv length) #x3FFFFFFF))))))
    (list run-count total-len weighted-sum curv-digest)))

(defun main ()
  (run-case *name* *description* *default-size* *default-seed* #'prepare #'run-kernel))

(eval-when (:execute)
  (main))
