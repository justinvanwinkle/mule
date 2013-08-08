
(DEFPACKAGE "mixed"
  (:USE "CL" "SB-EXT"))
(IN-PACKAGE "mixed")
(REQUIRE 'ASDF)
(IF (NOT (EQUAL (PACKAGE-NAME *PACKAGE*) "builtins"))
    (ASDF/OPERATE:LOAD-SYSTEM :MULE))
(DEFCLASS |CounterX| NIL (|count|))
(DEFMETHOD |__init__| ((|self| |CounterX|))
  (SETF (SLOT-VALUE |self| '|count|) 0))
(DEFMETHOD |incr| ((|self| |CounterX|))
  (SETF (SLOT-VALUE |self| '|count|) (+ (SLOT-VALUE |self| '|count|) 1)))
(DEFUN |CounterX| ()
  (LET ((|self| (MAKE-INSTANCE '|CounterX|)))
    |self|))
(DEFUN FOO ()
  (LET ((|c| (|CounterX|)))
    (LOOP WHILE (< (SLOT-VALUE |c| '|count|) 10000000)
          DO (|incr| |c|))
    (RETURN-FROM |foo| (SLOT-VALUE |c| '|count|))))
(DEFPARAMETER |x| (|foo|))
|print|
|x|
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
      WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
      DO (EXPORT S))