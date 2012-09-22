(require 'asdf)
(pushnew "/home/jvanwink/repos/cl-python/" asdf:*central-registry*)

(asdf:operate 'asdf:load-op 'clpython)


(defpackage :pycl.compile
  (:use
   :common-lisp
   :clpython.ast.node
   :clpython.ast.token
   :clpython.user))


(defun swap-symbol (s)
  (case s
    ((clpython.ast.operator:+) 'plus)
    (otherwise s)))


(defun translate (tree)
  (when (not (null tree))
    (if (listp (car tree))
        (translate (car tree))
      (setf (car tree) (swap-symbol (car tree))))
    (translate (cdr tree)))
  tree)

(defmacro |module-stmt| (&body body)
  (car body))

(defmacro |suite-stmt| (&rest (body))
  `(progn ,@body))

(defmacro |assign-stmt| (&rest body)
  `(destructuring-bind ,(mapcar #'second (second body)) (list ,(car body))))

(defmacro |binary-expr| (&rest body)
  body)

(defmacro |literal-expr| (&rest body)
  (when (eql :number (car body))
    (second body)))

(defmacro |identifier-expr| (&rest body)
  (car body))

(defmacro plus (&rest body)
  `(+ ,@body))

(defun pycl (filename)
  (let ((tree (clpython.parser:parse
               (open filename))))
    tree))

(defun eval-pycl (filename)
  (let ((tree (pycl filename)))
    (eval (translate tree))))

(eval-pycl #p"/home/jvanwink/repos/pycl/test_cases/test_assign.py")

;(translate (pycl #p"/home/jvanwink/repos/pycl/test_cases/test_assign.py"))
;; (MODULE-STMT
;;   (SUITE-STMT
;;    ((ASSIGN-STMT
;;      (BINARY-EXPR #1=# (LITERAL-EXPR :NUMBER 1) (LITERAL-EXPR :NUMBER 1))
;;      ((IDENTIFIER-EXPR CLPYTHON.USER::|x|)))
;;     (ASSIGN-STMT
;;      (BINARY-EXPR #1# (LITERAL-EXPR :NUMBER 2) (LITERAL-EXPR :NUMBER 2))
;;      ((IDENTIFIER-EXPR CLPYTHON.USER::|y|))))))
