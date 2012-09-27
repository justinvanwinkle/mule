(require 'asdf)
(pushnew "/home/jvanwink/repos/cl-python/" asdf:*central-registry*)

(asdf:operate 'asdf:load-op 'clpython)

;; (MODULE-STMT
;;   (SUITE-STMT
;;    ((ASSIGN-STMT
;;      (BINARY-EXPR #1=# (LITERAL-EXPR :NUMBER 1) (LITERAL-EXPR :NUMBER 1))
;;      ((IDENTIFIER-EXPR CLPYTHON.USER::|x|)))
;;     (ASSIGN-STMT
;;      (BINARY-EXPR #1# (LITERAL-EXPR :NUMBER 2) (LITERAL-EXPR :NUMBER 2))
;;      ((IDENTIFIER-EXPR CLPYTHON.USER::|y|))))))

(defpackage :pycl.compile
  (:use
   :common-lisp
   :clpython.ast.node
   :clpython.ast.token
   :clpython.user))

(defvar dispatch-table (make-hash-table))

(defun id (node)
  node)

(defun visit (node)
  (let* ((func (gethash (car node) dispatch-table 'id))
         (result (funcall func node)))
    (format t "using: ~a to visit ~%~a~%giving~%~a~%" func node result)
    result))

(defun module-v (node)
  (visit (second node)))
(setf (gethash 'clpython.ast.node::|module-stmt| dispatch-table) 'module-v)

(defun suite-eat (nodes)
  (if (null nodes)
      nil
      (let ((current (car nodes))
            (rest (cdr nodes)))
        (if (eql (car current) 'clpython.ast.node::|assign-stmt|)
            (assign-v current (suite-eat rest))
            (list 'progn (visit current) (suite-eat rest))))))

(defun suite-v (node)
  (suite-eat (second node)))
(setf (gethash 'clpython.ast.node::|suite-stmt| dispatch-table) 'suite-v)

(defun assign-v (node body)
  (let ((assign-form (third node))
        (assign-expr (second node)))
    (if (eql (length assign-form) 1)
        (list 'let (list (second assign-form) (visit assign-expr)) body)
        (error "multiple assignment"))))
(setf (gethash 'clpython.ast.node::|assign-stmt| dispatch-table) 'assign-v)

(defun binary-v (node)
  (let ((op (second node))
        (x (third node))
        (y (fourth node)))
    (list (gethash op dispatch-table) (visit x) (visit y))))
(setf (gethash 'clpython.ast.node::|binary-expr| dispatch-table) 'binary-v)
(setf (gethash 'clpython.ast.operator:+ dispatch-table) '+)

(defun literal-v (node)
  (third node))
(setf (gethash 'clpython.ast.token::|literal-expr| dispatch-table) 'literal-v)

(defun identifier-v (node)
  (
(setf (gethash 'CLPYTHON.AST.NODE:|identifier-expr| dispatch-table) 'identifier-v)

(defun pycl (filename)
  (let ((tree (clpython.parser:parse
               (open filename))))
    tree))

(defun eval-pycl (filename)
  (let ((tree (pycl filename)))
    (eval (translate tree))))

;(eval-pycl #p"/home/jvanwink/repos/pycl/test_cases/test_assign.py")

(visit (pycl #p"/home/jvanwink/repos/pycl/test_cases/test_assign.py"))
