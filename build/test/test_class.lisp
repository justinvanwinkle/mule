(SETF *READTABLE* (COPY-READTABLE NIL))(SETF (READTABLE-CASE *READTABLE*) :PRESERVE)(DEFPACKAGE "test_class" (:USE "CL" "SB-EXT" "SB-C"))(IN-PACKAGE "test_class")(DEFPARAMETER a 1) (DEFCLASS A () (a b)) (DEFMETHOD init ((self A) a  ) (SETF (SLOT-VALUE self 'a) a)) (DEFMETHOD meth ((self A)   ) (RETURN-FROM meth (+ (SLOT-VALUE self 'a) 1))) (DEFUN A (a)
                    (LET ((self (MAKE-INSTANCE 'A)))
                       (init self a)
                       self)) (SETF a (A 1 )) (ASSERT (EQUALP (SLOT-VALUE a 'a) 1) ) (ASSERT (EQUALP (meth a ) 2) )
