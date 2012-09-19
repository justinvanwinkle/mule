(require 'asdf)
(pushnew "/home/jvanwink/repos/cl-python/" asdf:*central-registry*)

(asdf:operate 'asdf:load-op 'clpython)

(defun swap-symbol (s)
  (case s
    ((clpython.ast.node:|module-stmt|) 'module-stmt)
    ((clpython.ast.node:|suite-stmt|) 'suite-stmt)
    ((clpython.ast.node:|assign-stmt|) 'assign-stmt)
    ((clpython.ast.node:|binary-expr|) 'binary-expr)
    ((clpython.ast.token:|literal-expr|) 'literal-expr)
    ((clpython.ast.node:|identifier-expr|) 'identifier-expr)
    ((clpython.ast.operator:+) '+)
    (otherwise s)))


(defun translate (tree)
  (when (not (null tree))
    (if (listp (car tree))
        (translate (car tree))
      (setf (car tree) (swap-symbol (car tree))))
    (translate (cdr tree)))
  tree)

(defun module-stmt (&rest body)
  (format t "called")
  body)

(defun suite-stmt (&rest body)
  body)

(defun assign-stmt (&rest body)
  body)

(defun binary-expr (&rest body)
  body)

(defun literal-expr (&rest body)
  body)

(defun identifier-expr (&rest body)
  (second body))

(defun pycl (filename)
  (let ((tree (clpython.parser:parse
               (open filename))))
    tree))

(defun eval-pycl (filename)
  (let ((tree (pycl filename)))
    (eval tree)))

(format t "~%~%~a~%" (pycl #p"/home/jvanwink/repos/pycl/test_cases/test_assign.py"))

(translate (pycl #p"/home/jvanwink/repos/pycl/test_cases/test_assign.py"))
