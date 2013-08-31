(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "import")(make-package "import")(use-package "builtins")))
;(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(|mload| "./dictionary" )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

