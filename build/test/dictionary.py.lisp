
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (UNLESS (FIND-PACKAGE "dictionary.py")
    (MAKE-PACKAGE "dictionary.py" :USE '("COMMON-LISP"))))
(IN-PACKAGE "dictionary.py")
(IF (NOT (EQUAL (PACKAGE-NAME *PACKAGE*) "builtins"))
    (LOAD "/home/jvanwink/repos/mule/build/lib/builtins.fasl"))
(USE-PACKAGE "builtins")
(DEFPARAMETER |d| (|dict|))
(|setitem| |d| 1 2)
(ASSERT (EQUALP (|getitem| |d| 1) 2))
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
      WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
      DO (EXPORT S))