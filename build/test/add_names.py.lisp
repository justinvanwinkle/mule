(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "add_names.py")(make-package "add_names.py" :use '("COMMON-LISP"))) ) (in-package "add_names.py")
(use-package "builtins")(DEFUN |test| (  ) (LET ((|x| 1)) (LET ((|y| 2)) (ASSERT (EQUALP (+ |x| |y|) 3) ))))
(|test|  )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

