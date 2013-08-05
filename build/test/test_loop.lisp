
(SETF *READTABLE* (COPY-READTABLE NIL))
(SETF (READTABLE-CASE *READTABLE*) :PRESERVE)
(DEFPACKAGE "test_loop"
  (:USE "CL" "SB-EXT"))
(IN-PACKAGE "test_loop")
(DEFUN simple_loop ()
  (LOOP FOR x BEING THE ELEMENTS OF (tuple-XXX XXX)
        DO (PROGN (print x) (print (+ x x)))))