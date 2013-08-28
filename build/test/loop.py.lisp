
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (UNLESS (FIND-PACKAGE "loop.py")
    (MAKE-PACKAGE "loop.py" :USE '("COMMON-LISP"))))
(IN-PACKAGE "loop.py")
(IF (NOT (EQUAL (PACKAGE-NAME *PACKAGE*) "builtins"))
    (LOAD "/home/jvanwink/repos/mule/build/lib/builtins.fasl"))
(USE-PACKAGE "builtins")
(DEFUN |simple_loop| ()
  (LOOP FOR |x| BEING THE ELEMENTS OF (|tuple| '(1 2 3))
        DO (PROGN (|print| |x|) (|print| (+ |x| |x|)))))
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
      WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
      DO (EXPORT S))