
(EVAL-WHEN (:COMPILE-TOPLEVEL)
  (SETF *READTABLE* (COPY-READTABLE NIL))
  (SETF (READTABLE-CASE *READTABLE*) :PRESERVE))
(DEFPACKAGE "global_scope"
  (:USE "CL" "SB-EXT"))
(IN-PACKAGE "global_scope")
(DEFPARAMETER x 1)
(+ x 1)
(+ x 2)