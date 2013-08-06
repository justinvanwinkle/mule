
(SETF *READTABLE* (COPY-READTABLE NIL))
(SETF (READTABLE-CASE *READTABLE*) :PRESERVE)
(DEFPACKAGE "add_names"
  (:USE "CL" "SB-EXT"))
(IN-PACKAGE "add_names")
(DEFUN test ()
  (LET ((x 1))
    (LET ((y 2))
      (+ x y))))