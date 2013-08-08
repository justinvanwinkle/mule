(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "splat")(make-package "splat" :use '("COMMON-LISP"))) ) (in-package "splat")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(DEFUN |f| (|a|  &REST |args|) (LET ((|x| |a|)) (LOOP FOR |arg| BEING THE ELEMENTS OF |args| DO (SETF |x| (+ |x| |arg|))) (RETURN-FROM |f| |x|))) (|ASSERT| (EQUALP (|f| 1 1 2 ) 4) ) (|ASSERT| (EQUALP (|f| 1 ) 1) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

