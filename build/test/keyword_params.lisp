(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "keyword_params")(make-package "keyword_params" :use '("COMMON-LISP"))) ) (in-package "keyword_params")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(DEFUN |f| (|a| &KEY (|b| "yep") ) (RETURN-FROM |f| |b|)) (|ASSERT| (EQUALP (|f| 1 ) "yep") ) (|ASSERT| (EQUALP (|f| 1 :|b| "NO") "NO") )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

