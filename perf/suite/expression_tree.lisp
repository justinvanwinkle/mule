;;;; Evaluate recursive expression trees with lexical Let bindings.

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load #.(merge-pathnames "perfutils/case.lisp"
                           (or *compile-file-truename* *load-truename*))
        :verbose nil :print nil))

(defparameter *name* "expression_tree")
(defparameter *description* "Recursive AST evaluation with polymorphic nodes and dict environments")
(defparameter *default-size* 40000)
(defparameter *default-seed* 42)

(defclass expr () ())

(defclass lit-expr (expr)
  ((value :initarg :value :reader lit-expr-value :type fixnum)))

(defclass var-expr (expr)
  ((name :initarg :name :reader var-expr-name :type simple-string)))

(defclass add-expr (expr)
  ((left :initarg :left :reader add-expr-left :type expr)
   (right :initarg :right :reader add-expr-right :type expr)))

(defclass mul-expr (expr)
  ((left :initarg :left :reader mul-expr-left :type expr)
   (right :initarg :right :reader mul-expr-right :type expr)))

(defclass let-expr (expr)
  ((name :initarg :name :reader let-expr-name :type simple-string)
   (bound :initarg :bound :reader let-expr-bound :type expr)
   (body :initarg :body :reader let-expr-body :type expr)))

(defgeneric eval-node (node env))

(defmethod eval-node ((node expr) (env hash-table))
  0)

(defmethod eval-node ((node lit-expr) (env hash-table))
  (the fixnum (lit-expr-value node)))

(defmethod eval-node ((node var-expr) (env hash-table))
  (the fixnum (gethash (var-expr-name node) env 0)))

(defmethod eval-node ((node add-expr) (env hash-table))
  (logand (the fixnum (+ (the fixnum (eval-node (add-expr-left node) env))
                         (the fixnum (eval-node (add-expr-right node) env))))
          #x3FFFFFFF))

(defmethod eval-node ((node mul-expr) (env hash-table))
  (logand (the fixnum (* (the fixnum (eval-node (mul-expr-left node) env))
                         (the fixnum (eval-node (mul-expr-right node) env))))
          #x3FFFFFFF))

(defmethod eval-node ((node let-expr) (env hash-table))
  (let* ((val (the fixnum (eval-node (let-expr-bound node) env)))
         (name (let-expr-name node)))
    (multiple-value-bind (old-val had-old) (gethash name env 0)
      (setf (gethash name env) val)
      (let ((res (the fixnum (eval-node (let-expr-body node) env))))
        (if had-old
            (setf (gethash name env) old-val)
            (remhash name env))
        res))))

(defun build-tree (depth seed)
  (declare (type fixnum depth seed))
  (if (<= depth 0)
      (let ((kind (mod seed 3)))
        (cond
          ((= kind 0) (make-instance 'lit-expr :value (+ (mod seed 19) 1)))
          ((= kind 1) (make-instance 'var-expr :name "x"))
          (t (make-instance 'var-expr :name "y"))))
      (let* ((s1 (logand (+ (* seed 1103515245) 12345) #x7FFFFFFF))
             (s2 (logand (+ (* s1 1103515245) 12345) #x7FFFFFFF))
             (mode (mod seed 3)))
        (cond
          ((= mode 0)
           (make-instance 'add-expr
                          :left (build-tree (- depth 1) s1)
                          :right (build-tree (- depth 1) s2)))
          ((= mode 1)
           (make-instance 'mul-expr
                          :left (build-tree (- depth 1) s1)
                          :right (build-tree (- depth 1) s2)))
          (t
           (let ((var (if (= (logand seed 4) 0) "x" "y")))
             (make-instance 'let-expr
                            :name var
                            :bound (make-instance 'add-expr
                                                  :left (build-tree (- depth 2) s1)
                                                  :right (make-instance 'lit-expr :value 3))
                            :body (build-tree (- depth 1) s2))))))))

(defstruct (eval-input (:constructor make-eval-input (tree-idx x-val y-val)))
  (tree-idx 0 :type fixnum)
  (x-val 0 :type fixnum)
  (y-val 0 :type fixnum))

(defun prepare (size seed)
  (declare (type fixnum size seed))
  (when (< size 0)
    (error "size must be nonnegative"))
  (let ((state (logand seed #x7FFFFFFF))
        (inputs (make-array size)))
    (declare (type fixnum state)
             (type simple-vector inputs))
    (dotimes (i size inputs)
      (declare (type fixnum i))
      (setf state (logand (+ (* state 1103515245) 12345) #x7FFFFFFF))
      (let ((tree-idx (logand (ash state -4) 3))
            (x-val (logand (ash state -8) 31))
            (y-val (logand (ash state -13) 31)))
        (setf (svref inputs i) (make-eval-input tree-idx x-val y-val))))))

(defun run-kernel (data)
  (declare (type simple-vector data))
  (let ((trees (vector (build-tree 4 17)
                       (build-tree 4 53)
                       (build-tree 4 97)
                       (build-tree 4 131)))
        (env (make-hash-table :test 'equal))
        (n (length data)))
    (declare (type simple-vector trees)
             (type fixnum n))
    (setf (gethash "x" env) 0
          (gethash "y" env) 0)
    (let ((rounds (max 1 (floor n 465)))
          (checksum 0)
          (high-count 0))
      (declare (type fixnum rounds checksum high-count))
      (dotimes (round-idx rounds)
        (declare (type fixnum round-idx))
        (let ((bias (logand round-idx 7)))
          (declare (type fixnum bias))
          (dotimes (i n)
            (declare (type fixnum i))
            (let* ((inp (the eval-input (svref data i)))
                   (tree-idx (eval-input-tree-idx inp))
                   (x-val (eval-input-x-val inp))
                   (y-val (eval-input-y-val inp)))
              (setf (gethash "x" env) (the fixnum (+ x-val bias)))
              (setf (gethash "y" env) (the fixnum (logxor y-val bias)))
              (let ((value (the fixnum
                                (eval-node (svref trees (logand (+ tree-idx round-idx) 3))
                                           env))))
                (declare (type fixnum value))
                (setf checksum (logand (the fixnum (+ checksum value)) #x3FFFFFFF))
                (when (> value 1000)
                  (incf high-count)))))))
      (list checksum high-count))))

(defun main ()
  (run-case *name* *description* *default-size* *default-seed* #'prepare #'run-kernel))

(eval-when (:execute)
  (main))
