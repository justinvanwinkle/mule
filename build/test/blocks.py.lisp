(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "blocks.py")(make-package "blocks.py" :use '("COMMON-LISP"))(use-package "builtins")))
(DEFUN |x| (  ) (FLET ((|y| (  ) (FLET ((|z| (  ) (RETURN-FROM |z| 1))) (RETURN-FROM |y| (+ 2 (|z|  )))))) (RETURN-FROM |x| (+ 3 (|y|  )))))
(ASSERT (|__eq__| (|x|  ) 6) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

