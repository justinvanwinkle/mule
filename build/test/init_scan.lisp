
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (UNLESS (FIND-PACKAGE "init_scan")
    (MAKE-PACKAGE "init_scan" :USE '("COMMON-LISP"))))
(IN-PACKAGE "init_scan")
(REQUIRE 'ASDF)
(IF (NOT (EQUAL (PACKAGE-NAME *PACKAGE*) "builtins"))
    (ASDF/OPERATE:LOAD-SYSTEM :MULE))
(USE-PACKAGE "builtins")
(DEFCLASS A NIL (|a| |b| |c| |d|))
(DEFUN A ()
  (LET ((|self| (MAKE-INSTANCE 'A)))
    (SETF (SLOT-VALUE |self| '|a|) 1)
    (COND (1 (SETF (SLOT-VALUE |self| '|b|) 2)))
    (LOOP WHILE T
          DO (SETF (SLOT-VALUE |self| '|c|) 3))
    (LOOP FOR |x| FROM 0 BELOW 10 BY 1
          DO (SETF (SLOT-VALUE |self| '|d|) "xxx"))
    |self|))
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
      WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
      DO (EXPORT S))