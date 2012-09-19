(PROGN
 (IN-PACKAGE :CLPYTHON)
 (CLPYTHON.AST.NODE:|module-stmt|
  (CLPYTHON.AST.NODE:|suite-stmt|
   ((CLPYTHON.AST.NODE:|assign-stmt|
     (CLPYTHON.AST.NODE:|binary-expr| CLPYTHON.AST.OPERATOR:+
                                      (CLPYTHON.AST.TOKEN:|literal-expr|
                                       :NUMBER 1)
                                      (CLPYTHON.AST.TOKEN:|literal-expr|
                                       :NUMBER 1))
     ((CLPYTHON.AST.NODE:|identifier-expr| CLPYTHON.USER::|x|)))))))
