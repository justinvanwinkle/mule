
(SETF *READTABLE* (COPY-READTABLE NIL))
(SETF (READTABLE-CASE *READTABLE*) :PRESERVE)
(DEFPACKAGE "test_tuple"
  (:USE "CL" "SB-EXT"))
(IN-PACKAGE "test_tuple")
(DEFPARAMETER x (tuple-XXX XXX))