;;;; Allocate, traverse, and discard recursive binary trees alongside a long-lived tree.

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load #.(merge-pathnames "perfutils/case.lisp"
                           (or *compile-file-truename* *load-truename*))
        :verbose nil :print nil))

(defparameter *name* "binary_trees")
(defparameter *description* "Recursive binary tree allocation, traversal, and deallocation churn")
(defparameter *default-size* 3500)
(defparameter *default-seed* 42)

(defstruct (tree-node (:constructor make-tree-node (item left right)))
  (item 0 :type fixnum)
  left
  right)

(defun prepare (size seed)
  (declare (type fixnum size seed))
  (when (< size 1)
    (error "size must be positive"))
  (let ((state (logand seed #x7FFFFFFF))
        (requests (make-array size)))
    (declare (type fixnum state)
             (type simple-vector requests))
    (dotimes (i size requests)
      (declare (type fixnum i))
      (setf state (logand (+ (* state 1103515245) 12345) #x7FFFFFFF))
      (let ((depth (+ 4 (mod (ash state -8) 5))))
        (setf state (logand (+ (* state 1103515245) 12345) #x7FFFFFFF))
        (let ((root-val (- (mod (ash state -8) 201) 100)))
          (setf (svref requests i) (cons depth root-val)))))))

(defun make-tree (item depth)
  (declare (type fixnum item depth))
  (if (<= depth 0)
      (make-tree-node item nil nil)
      (let ((next-depth (the fixnum (1- depth))))
        (make-tree-node item
                        (make-tree (the fixnum (1- (the fixnum (* item 2)))) next-depth)
                        (make-tree (the fixnum (1+ (the fixnum (* item 2)))) next-depth)))))

(defun check-tree (node)
  (declare (type tree-node node))
  (let ((item (tree-node-item node))
        (left (tree-node-left node)))
    (declare (type fixnum item))
    (if (null left)
        item
        (let ((right (tree-node-right node)))
          (the fixnum
               (- (the fixnum (+ item (the fixnum (check-tree left))))
                  (the fixnum (check-tree right))))))))

(defun run-kernel (data)
  (declare (type simple-vector data))
  (let ((stretch-check (check-tree (make-tree 1 10)))
        (long-lived (make-tree 3 9))
        (total-check 0)
        (total-nodes 0)
        (n (length data)))
    (declare (type fixnum stretch-check total-check total-nodes n))
    (dotimes (i n)
      (declare (type fixnum i))
      (let* ((req (svref data i))
             (depth (the fixnum (car req)))
             (root-val (the fixnum (cdr req)))
             (pos-tree (make-tree root-val depth))
             (neg-tree (make-tree (the fixnum (- root-val)) depth)))
        (incf total-check (the fixnum (+ (the fixnum (check-tree pos-tree))
                                         (the fixnum (check-tree neg-tree)))))
        (incf total-nodes (the fixnum (* (the fixnum (1- (the fixnum (ash 1 (the fixnum (1+ depth))))))
                                         2)))))
    (let ((long-check (the fixnum (+ (the fixnum (check-tree long-lived)) stretch-check))))
      (list total-check long-check total-nodes))))

(defun main ()
  (run-case *name* *description* *default-size* *default-seed* #'prepare #'run-kernel))

(eval-when (:execute)
  (main))
