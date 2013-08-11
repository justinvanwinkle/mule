(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "add_names")(make-package "add_names" :use '("COMMON-LISP"))) ) (in-package "add_names")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(use-package "builtins")(DEFUN |test| (  ) (LET ((|x| 1)) (LET ((|y| 2)) (|ASSERT| (EQUALP (+ |x| |y|) 3) )))) (|test|  )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

