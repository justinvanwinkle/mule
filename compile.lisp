(require 'asdf)
(pushnew "/home/jvanwink/repos/cl-python/" asdf:*central-registry*)

(asdf:operate 'asdf:load-op 'clpython)

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
    ; (format t "using: ~a to visit ~%~a~%giving~%~a~%" func node result)
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
            (if (null rest)
                (visit current)
                (list 'progn (visit current) (suite-eat rest)))))))

(defun suite-v (node)
  (suite-eat (second node)))
(setf (gethash 'clpython.ast.node::|suite-stmt| dispatch-table) 'suite-v)

(defun assign-v (node body)
  (let ((assign-form (third node))
        (assign-expr (second node)))
    ; (format t "ASSIGN ~a~%~a~%" assign-form assign-expr)
    (if (eql (length assign-form) 1)
        (list 'let (list (list (visit (first assign-form)) (visit assign-expr))) body)
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
  (second node))
(setf (gethash 'CLPYTHON.AST.NODE:|identifier-expr| dispatch-table) 'identifier-v)

(defun funcdef-v (node)
  (let ((funcname (visit (third node)))
        (arglist (mapcar 'visit (first (fourth node))))
        (body (visit (fifth node))))
    (list 'defun funcname arglist (list 'block nil body))))
(setf (gethash 'CLPYTHON.AST.NODE:|funcdef-stmt| dispatch-table) 'funcdef-v)

(defun return-v (node)
  (list 'return (visit (second node))))
(setf (gethash 'CLPYTHON.AST.NODE:|return-stmt| dispatch-table) 'return-v)

(defun call-v (node)
  (let ((form (list (visit (second node)))))
    (dolist (x (third node))
      (push (visit x) form))
    (reverse form)))
(setf (gethash 'CLPYTHON.AST.NODE:|call-expr| dispatch-table) 'call-v)

(defun print-v (node)
  (list 'format 't "~a~%" (visit (car (third node)))))
(setf (gethash 'CLPYTHON.AST.NODE:|print-stmt| dispatch-table) 'print-v)

(defun pycl (filename)
  (let ((tree (clpython.parser:parse
               (open filename))))
    tree))

(defun eval-pycl (filename)
  (let ((tree (pycl filename)))
    (eval (translate tree))))

;(eval-pycl #p"/home/jvanwink/repos/pycl/test_cases/test_assign.py")

(visit (pycl #p"/home/jvanwink/repos/pycl/test_cases/test_assign.py"))
