
(SETF *READTABLE* (COPY-READTABLE NIL))
(SETF (READTABLE-CASE *READTABLE*) :PRESERVE)
(DEFPACKAGE "loop"
  (:USE "CL" "SB-EXT"))
(IN-PACKAGE "loop")
(DEFUN simple_loop ()
  (LOOP FOR x BEING THE ELEMENTS OF (tuple-XXX XXX)
        DO (PROGN (print x) (print (+ x x)))))