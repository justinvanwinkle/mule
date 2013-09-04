(CL:EVAL-WHEN (:compile-toplevel :load-toplevel :execute)(CL:UNLESS (CL:FIND-PACKAGE "exceptions")(make-package "exceptions")(CL:USE-PACKAGE "builtins")))
;(proclaim '(optimize (speed 3)))
(|IN-PACKAGE| "builtins" )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:condition ) )
(|SETF| (|FIND-CLASS| '|ArithmeticError| ) (|FIND-CLASS| 'cl:arithmetic-error ) )
(|SETF| (|FIND-CLASS| '|FloatingPointError| ) (|FIND-CLASS| 'cl:floating-point-overflow ) )
(|SETF| (|FIND-CLASS| '|TypeError| ) (|FIND-CLASS| 'cl:type-error ) )
(|SETF| (|FIND-CLASS| '|AttributeError| ) (|FIND-CLASS| 'cl:simple-error ) )
(|SETF| (|FIND-CLASS| '|IOError| ) (|FIND-CLASS| 'cl:file-error ) )
(|SETF| (|FIND-CLASS| '|ZeroDivisionError| ) (|FIND-CLASS| 'cl:division-by-zero ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:control-error ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:end-of-file ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:error ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:floating-point-inexact ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:floating-point-invalid-operation ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:floating-point-underflow ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:package-error ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:parse-error ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:print-not-readable ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:program-error ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:reader-error ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:serious-condition ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:simple-condition ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:simple-warning ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:storage-condition ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:stream-error ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:style-warning ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:unbound-slot ) )
(|SETF| (|FIND-CLASS| '|NameError| ) (|FIND-CLASS| 'cl:unbound-variable ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:undefined-function ) )
(|SETF| (|FIND-CLASS| '|Exception| ) (|FIND-CLASS| 'cl:warning ) )
(CL:DEFINE-CONDITION |AssertionError| (CL:SIMPLE-ERROR) NIL)
(CL:DEFINE-CONDITION |KeyError| (CL:ERROR) NIL)
#| IOError
|#
#| ImportError
|#
#| ImportWarning
|#
#| IndentationError
|#
#| IndexError
|#
#| KeyError
|#
#| KeyboardInterrupt
|#
#| LookupError
|#
#| MemoryError
|#
#| NameError
|#
#| NotImplementedError
|#
#| OSError
|#
#| OverflowError
|#
#| PendingDeprecationWarning
|#
#| ReferenceError
|#
#| RuntimeError
|#
#| RuntimeWarning
|#
#| StandardError
|#
#| StopIteration
|#
#| SyntaxError
|#
#| SyntaxWarning
|#
#| SystemError
|#
#| SystemExit
|#
#| TabError
|#
#| TypeError
|#
#| UnboundLocalError
|#
#| UnicodeDecodeError
|#
#| UnicodeEncodeError
|#
#| UnicodeError
|#
#| UnicodeTranslateError
|#
#| UnicodeWarning
|#
#| UserWarning
|#
#| ValueError
|#
#| Warning
|#
(CL:LOOP FOR S BEING EACH PRESENT-SYMBOL IN CL:*PACKAGE*
   WHEN (OR (CL:FBOUNDP S)
            (CL:BOUNDP S)
            (CL:FIND-CLASS S NIL))
   DO (CL:EXPORT S))

