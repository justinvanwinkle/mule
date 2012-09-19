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

(defmacro module-stmt (&body body)
  `(progn ,@body))

(defmacro suite-stmt (&body body)
  `(progn ,@(car body)))

(defmacro assign-stmt (&rest body)
  `( ,@body))

(defmacro  binary-expr (&rest body)
  body)

(defmacro literal-expr (&rest body)
  (when (eql :number (car body))
    (second body)))

(defmacro identifier-expr (&rest body)
  (car body))

(defun pycl (filename)
  (let ((tree (clpython.parser:parse
               (open filename))))
    tree))

(defun eval-pycl (filename)
  (let ((tree (pycl filename)))
    (eval (translate tree))))

(eval-pycl #p"/home/jvanwink/repos/pycl/test_cases/test_assign.py")

(translate (pycl #p"/home/jvanwink/repos/pycl/test_cases/test_assign.py"))
