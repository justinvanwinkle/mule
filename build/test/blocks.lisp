
(EVAL-WHEN (:COMPILE-TOPLEVEL)
  (SETF *READTABLE* (COPY-READTABLE NIL))
  (SETF (READTABLE-CASE *READTABLE*) :PRESERVE))
(DEFPACKAGE "blocks"
  (:USE "CL" "SB-EXT"))
(IN-PACKAGE "blocks")
(DEFUN x ()
  (FLET ((y ()
           (FLET ((z ()
                    (RETURN-FROM z 1)))
             (RETURN-FROM y (+ 2 (z))))))
    (RETURN-FROM x (+ 3 (y)))))
(ASSERT (EQUALP (x) 6))