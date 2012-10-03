(require 'asdf)
(pushnew "/home/jvanwink/repos/cl-python/" asdf:*central-registry*)

(asdf:operate 'asdf:load-op 'clpython)

(defpackage :pycl.compile
  (:use :common-lisp
   :clpython.ast.node
   :clpython.ast.token
   :clpython.user))


(defclass node-visitor ()
  ((namespace-stack
    :initform nil)))

(defgeneric push-namespace (node-v)
  (:method ((node-v node-visitor))
    (with-slots (namespace-stack) node-v
      (push nil namespace-stack))))

(defgeneric pop-namespace (node-v)
  (:method ((node-v node-visitor))
    (with-slots (namespace-stack) node-v
      (pop namespace-stack))))

(defgeneric create-name (node-v name)
  (:method ((node-v node-visitor) name)
    (with-slots (namespace-stack) node-v
      (push name (car namespace-stack)))))

(defgeneric name-exists (node-v name)
  (:method ((node-v node-visitor) name)
    (with-slots (namespace-stack) node-v
      (mapcan (lambda (x) (member name x)) namespace-stack))))

(defgeneric namespace-depth (node-v)
  (:method ((node-v node-visitor))
    (with-slots (namespace-stack) node-v
      (length namespace-stack))))

(defgeneric visit (node-v node)
  (:method ((node-v node-visitor) node)
    (let* ((tag (car node))
           (result (compile-form node-v tag node)))
      (format t "~%using: ~a to visit ~%~a~%giving~%~a~%" tag node result)
      result)))

(defun python-plus (&optional args)
  (if (every #'numberp args)
      (reduce #'+ args)))

(defun assign-names (node-v node)
  (visit node-v (first (third node))))

(defun use-let (node-v node)
  (if (= (namespace-depth node-v) 1)
      nil
      t))

(defun assign-stmt (node-v node &optional body)
  (let ((assign-target (third node))
        (assign-expr (second node)))
    (dolist (name assign-target)
      (create-name node-v (visit node-v name)))
    (if (eql (length assign-target) 1)
        (if (use-let node-v node)
            (list 'let (list (list (visit node-v (car assign-target))
                                   (visit node-v assign-expr)))
                             body)
            (list 'defparameter (visit node-v (car assign-target))
                  (visit node-v assign-expr)))

        (error "multiple assignment"))))

(defun recurse-suite (node-v nodes)
  (format t "RECURSE ~%~a~%" nodes)
  (if (null nodes)
      nil
      (let ((current (car nodes))
            (rest (cdr nodes)))
        (if (eql (car current) 'clpython.ast.node:|assign-stmt|)
            (if (use-let node-v current)
                (assign-stmt node-v current (recurse-suite node-v rest))
                (append (list (assign-stmt node-v current)) (recurse-suite node-v rest)))
            (if (null rest)
                (visit node-v current)
                (list (visit node-v current) (recurse-suite node-v rest)))))))

(defgeneric compile-form (node-v tag node)
  (:method ((node-v node-visitor) tag node)
    node)

  (:method ((node-v node-visitor) (tag (eql 'clpython.ast.node:|module-stmt|)) node)
    (push-namespace node-v)
    (let ((result (visit node-v (second node))))
      (pop-namespace node-v)
      result))

  (:method ((node-v node-visitor) (tag (eql 'clpython.ast.node:|binary-expr|)) node)
    (let ((op (second node))
          (x (third node))
          (y (fourth node)))
      (list op (visit node-v x) (visit node-v y))))

  (:method ((node-v node-visitor) (tag (eql 'clpython.ast.token:|literal-expr|)) node)
    (third node))

  (:method :before
    ((node-v node-visitor) (tag (eql 'clpython.ast.node:|funcdef-stmt|)) node)
    (push-namespace node-v))

  (:method ((node-v node-visitor) (tag (eql 'clpython.ast.node:|funcdef-stmt|)) node)
    (let ((funcname (visit node-v (third node)))
          (arglist (mapcar (lambda (x) (visit node-v x)) (first (fourth node))))
          (body (visit node-v (fifth node))))
      (list 'defun funcname arglist (list 'block nil body))))

  (:method :after
    ((node-v node-visitor) (tag (eql 'clpython.ast.node:|funcdef-stmt|)) node)
    (pop-namespace node-v))

  (:method ((node-v node-visitor) (tag (eql 'clpython.ast.node:|identifier-expr|)) node)
    (second node))

  (:method ((node-v node-visitor) (tag (eql 'clpython.ast.node:|return-stmt|)) node)
    (list 'return (visit node-v (second node))))

  (:method ((node-v node-visitor) (tag (eql 'clpython.ast.node:|call-expr|)) node)
    (let ((form (list (visit node-v (second node)))))
      (dolist (x (third node))
        (push (visit node-v x) form))
      (reverse form)))

  (:method ((node-v node-visitor) (tag (eql 'clpython.ast.node:|print-stmt|)) node)
    (list 'format 't "~a~%" (visit node-v (car (third node)))))

  (:method ((node-v node-visitor) (tag (eql 'clpython.ast.node:|suite-stmt|)) node)
    (recurse-suite node-v (second node)))

  (:method ((node-v node-visitor) (tag (eql 'clpython.ast.node:|assign-stmt|)) node)
    (let ((assign-form (third node))
          (assign-expr (second node)))
      (if (= (length assign-form) 1)
          (list 'let (list (list (visit node-v (first assign-form))
                                 (visit node-v assign-expr))))
          (error "multiple assignment"))))

  (:method ((node-v node-visitor) (tag (eql 'CLPYTHON.AST.OPERATOR:+)) node)
    (list 'python-plus (cdr node))))

(defun pycl (filename)
  (let ((tree (clpython.parser:parse
               (open filename))))
    tree))

(defun eval-pycl (filename)
  (let ((tree (pycl filename)))
    (eval (translate tree))))

;(eval-pycl #p"/home/jvanwink/repos/pycl/test_cases/test_assign.py")

;(visit (make-instance 'node-visitor) (pycl #p"/home/jvanwink/repos/pycl/test_cases/test_assign.py"))
