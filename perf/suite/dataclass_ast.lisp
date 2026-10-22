;;;; Construct, algebraically simplify, and evaluate AST records.

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load #.(merge-pathnames "perfutils/case.lisp"
                           (or *compile-file-truename* *load-truename*))
        :verbose nil :print nil))

(defparameter *name* "dataclass_ast")
(defparameter *description* "Slotted AST records, match/case structural pattern matching, and tree rewriting")
(defparameter *default-size* 4000)
(defparameter *default-seed* 42)

(defstruct (lit-node (:constructor make-lit (value)))
  (value 0 :type fixnum))

(defstruct (var-node (:constructor make-var (slot)))
  (slot 0 :type fixnum))

(defstruct (unary-node (:constructor make-unary (op operand)))
  (op 0 :type fixnum)
  operand)

(defstruct (binary-node (:constructor make-binary (op left right)))
  (op 0 :type fixnum)
  left
  right)

(defstruct (select-node (:constructor make-select (cond then-branch else-branch)))
  cond
  then-branch
  else-branch)

(defun next-lcg (state)
  (declare (type fixnum state))
  (the fixnum (logand (+ (* state 1103515245) 12345) #x7FFFFFFF)))

(defun build-tree (depth state)
  (declare (type fixnum depth state))
  (setf state (next-lcg state))
  (if (<= depth 0)
      (if (zerop (logand state 3))
          (values (make-var (logand (ash state -4) 3)) state)
          (values (make-lit (- (mod (ash state -8) 19) 9)) state))
      (let ((kind (mod (ash state -4) 7))
            (next-depth (1- depth)))
        (declare (type fixnum kind next-depth))
        (cond
          ((= kind 0)
           (multiple-value-bind (inner s1) (build-tree next-depth state)
             (values (make-unary 1 (make-unary 1 inner)) s1)))
          ((= kind 1)
           (multiple-value-bind (left s1) (build-tree next-depth state)
             (values (make-binary 1 left (make-lit 0)) s1)))
          ((= kind 2)
           (multiple-value-bind (right s1) (build-tree next-depth state)
             (values (make-binary 3 (make-lit 1) right) s1)))
          ((= kind 3)
           (setf state (next-lcg state))
           (let ((a (- (mod (ash state -8) 15) 7))
                 (b (- (mod (ash state -16) 15) 7)))
             (multiple-value-bind (sub s1) (build-tree next-depth state)
               (values (make-binary 1 (make-binary 1 (make-lit a) (make-lit b)) sub) s1))))
          ((= kind 4)
           (let ((cond-val (logand (ash state -9) 1)))
             (multiple-value-bind (then-b s1) (build-tree next-depth state)
               (multiple-value-bind (else-b s2) (build-tree next-depth s1)
                 (values (make-select (make-lit cond-val) then-b else-b) s2)))))
          ((= kind 5)
           (multiple-value-bind (c-node s1) (build-tree next-depth state)
             (multiple-value-bind (then-b s2) (build-tree next-depth s1)
               (multiple-value-bind (else-b s3) (build-tree next-depth s2)
                 (values (make-select c-node then-b else-b) s3)))))
          (t
           (let ((op (+ (mod (ash state -8) 3) 1)))
             (multiple-value-bind (left s1) (build-tree next-depth state)
               (multiple-value-bind (right s2) (build-tree next-depth s1)
                 (values (make-binary op left right) s2)))))))))

(defun prepare (size seed)
  (declare (type fixnum size seed))
  (when (< size 1)
    (error "size must be positive"))
  (let ((state (logand seed #x7FFFFFFF))
        (tree-seeds (make-array size :element-type 'fixnum))
        (envs (make-array 8)))
    (declare (type fixnum state)
             (type (simple-array fixnum (*)) tree-seeds)
             (type simple-vector envs))
    (dotimes (i size)
      (declare (type fixnum i))
      (setf state (next-lcg state)
            (aref tree-seeds i) state))
    (dotimes (i 8 (cons tree-seeds envs))
      (declare (type fixnum i))
      (setf state (next-lcg state))
      (let ((env (make-array 4 :element-type 'fixnum)))
        (setf (aref env 0) (- (mod (ash state -4) 21) 10)
              (aref env 1) (- (mod (ash state -10) 21) 10)
              (aref env 2) (- (mod (ash state -16) 21) 10)
              (aref env 3) (- (mod (ash state -22) 21) 10))
        (setf (svref envs i) env)))))

(defun simplify (node)
  (etypecase node
    ((or lit-node var-node)
     node)
    (unary-node
     (let ((op (unary-node-op node))
           (inner (simplify (unary-node-operand node))))
       (declare (type fixnum op))
       (cond
         ((and (lit-node-p inner) (= op 1))
          (make-lit (- (lit-node-value inner))))
         ((and (lit-node-p inner) (= op 2))
          (let ((v (lit-node-value inner)))
            (make-lit (if (>= v 0) v (- v)))))
         ((and (unary-node-p inner) (= op 1) (= (unary-node-op inner) 1))
          (unary-node-operand inner))
         (t
          (make-unary op inner)))))
    (binary-node
     (let ((op (binary-node-op node))
           (left (simplify (binary-node-left node)))
           (right (simplify (binary-node-right node))))
       (declare (type fixnum op))
       (cond
         ((and (lit-node-p left) (lit-node-p right))
          (let ((a (lit-node-value left))
                (b (lit-node-value right)))
            (declare (type fixnum a b))
            (cond
              ((= op 1) (make-lit (+ a b)))
              ((= op 2) (make-lit (- a b)))
              (t (make-lit (* a b))))))
         ((and (= op 1) (lit-node-p right) (= (lit-node-value right) 0))
          left)
         ((and (= op 1) (lit-node-p left) (= (lit-node-value left) 0))
          right)
         ((and (= op 2) (lit-node-p right) (= (lit-node-value right) 0))
          left)
         ((and (= op 2) (lit-node-p left) (= (lit-node-value left) 0))
          (simplify (make-unary 1 right)))
         ((and (= op 3)
               (or (and (lit-node-p right) (= (lit-node-value right) 0))
                   (and (lit-node-p left) (= (lit-node-value left) 0))))
          (make-lit 0))
         ((and (= op 3) (lit-node-p right) (= (lit-node-value right) 1))
          left)
         ((and (= op 3) (lit-node-p left) (= (lit-node-value left) 1))
          right)
         (t
          (make-binary op left right)))))
    (select-node
     (let ((cond-node (simplify (select-node-cond node))))
       (if (lit-node-p cond-node)
           (if (/= (lit-node-value cond-node) 0)
               (simplify (select-node-then-branch node))
               (simplify (select-node-else-branch node)))
           (make-select cond-node
                        (simplify (select-node-then-branch node))
                        (simplify (select-node-else-branch node))))))))

(defun node-count (node)
  (etypecase node
    ((or lit-node var-node) 1)
    (unary-node
     (the fixnum (1+ (the fixnum (node-count (unary-node-operand node))))))
    (binary-node
     (the fixnum (+ 1
                    (the fixnum (node-count (binary-node-left node)))
                    (the fixnum (node-count (binary-node-right node))))))
    (select-node
     (the fixnum (+ 1
                    (the fixnum (node-count (select-node-cond node)))
                    (the fixnum (node-count (select-node-then-branch node)))
                    (the fixnum (node-count (select-node-else-branch node))))))))

(defun py-mod-pos (x m)
  (declare (type fixnum x m))
  (let ((r (rem x m)))
    (if (< r 0) (the fixnum (+ r m)) r)))

(defun eval-ast (node env)
  (declare (type (simple-array fixnum (4)) env))
  (etypecase node
    (lit-node
     (lit-node-value node))
    (var-node
     (aref env (var-node-slot node)))
    (unary-node
     (let ((val (the fixnum (eval-ast (unary-node-operand node) env))))
       (if (= (unary-node-op node) 1)
           (the fixnum (- val))
           (if (>= val 0) val (the fixnum (- val))))))
    (binary-node
     (let ((op (binary-node-op node))
           (lval (the fixnum (eval-ast (binary-node-left node) env)))
           (rval (the fixnum (eval-ast (binary-node-right node) env))))
       (declare (type fixnum op lval rval))
       (cond
         ((= op 1) (the fixnum (+ lval rval)))
         ((= op 2) (the fixnum (- lval rval)))
         (t (py-mod-pos (the fixnum (* lval rval)) 100003)))))
    (select-node
     (if (/= (the fixnum (eval-ast (select-node-cond node) env)) 0)
         (eval-ast (select-node-then-branch node) env)
         (eval-ast (select-node-else-branch node) env)))))

(defun run-kernel (data)
  (let* ((tree-seeds (the (simple-array fixnum (*)) (car data)))
         (envs (the simple-vector (cdr data)))
         (n-trees (length tree-seeds))
         (n-envs (length envs))
         (raw-nodes 0)
         (simplified-nodes 0)
         (eval-sum 0)
         (digest 0))
    (declare (type (simple-array fixnum (*)) tree-seeds)
             (type simple-vector envs)
             (type fixnum n-trees n-envs raw-nodes simplified-nodes eval-sum digest))
    (dotimes (i n-trees)
      (declare (type fixnum i))
      (let* ((tree (nth-value 0 (build-tree 4 (aref tree-seeds i))))
             (rewritten (simplify tree)))
        (incf raw-nodes (the fixnum (node-count tree)))
        (incf simplified-nodes (the fixnum (node-count rewritten)))
        (dotimes (j n-envs)
          (declare (type fixnum j))
          (let* ((env (the (simple-array fixnum (4)) (svref envs j)))
                 (val (the fixnum (eval-ast rewritten env))))
            (incf eval-sum val)
            (setf digest
                  (logand (+ (* digest 33) (logand val #xFFFF)) #x3FFFFFFF))))))
    (list raw-nodes simplified-nodes eval-sum digest)))

(defun main ()
  (run-case *name* *description* *default-size* *default-seed* #'prepare #'run-kernel))

(eval-when (:execute)
  (main))
