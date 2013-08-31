(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "dictionary.py")(make-package "dictionary.py")(use-package "builtins")))
;(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(DEFPARAMETER |d| (|dict|  ))
(|setitem| |d| 1 2)
(|setitem| |d| "hey" 3)
(|setitem| |d| 4.0 "yup")
(ASSERT (|__eq__| (|getitem| |d| 1) 2) )
(ASSERT (|__eq__| (|len| |d| ) 3) )
(ASSERT (|__eq__| (|sorted| (|keys| |d| ) ) (|sorted| (|list| '(1 "hey" 4.0)) )) )
(|setitem| |d| 4.0 1)
(|setitem| |d| 4 2)
(ASSERT (|__eq__| (|getitem| |d| 4.0) 2) )
(SETF |d| (|dict|  ))
(LOOP FOR |x| FROM 0 BELOW 100 BY 1 DO (|setitem| |d| |x| |x|))
(ASSERT (|__eq__| (|len| |d| ) 100) )
#| d2 = dict()
|#
#| for k in d:
|#
#|     d2[k] = d[k]
|#
#| assert d2 == d
|#
(SETF |d| (|dict|  ))
(|setitem| |d| 1 2)
(|setitem| |d| 3 4)
(|setitem| |d| 2 2)
(DEFPARAMETER |l1| (|sorted| (|keys| |d| ) ))
(DEFPARAMETER |l2| (|sorted| (|keys| |d| ) ))
(ASSERT (|__eq__| |l1| |l2|) )
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

