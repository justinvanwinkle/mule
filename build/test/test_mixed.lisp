(SETF *READTABLE* (COPY-READTABLE NIL))(SETF (READTABLE-CASE *READTABLE*) :PRESERVE)(DEFPACKAGE "test_mixed" (:USE "CL" "SB-EXT" "SB-C"))(IN-PACKAGE "test_mixed")(DEFCLASS CounterX () (count)) (DEFMETHOD __init__ ((self CounterX)   ) (SETF (SLOT-VALUE self 'count) 0)) (DEFMETHOD incr ((self CounterX)   ) (SETF (SLOT-VALUE self 'count) (+ (SLOT-VALUE self 'count) 1))) (DEFUN CounterX ()
                    (LET ((self (MAKE-INSTANCE 'CounterX)))
                       
                       self)) (DEFUN foo (  ) (LET ((c (CounterX  ))) (LOOP WHILE (< (SLOT-VALUE c 'count) 10000000) DO (incr c )) (RETURN-FROM foo (SLOT-VALUE c 'count)))) (DEFPARAMETER x (foo  )) print x
