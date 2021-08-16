(CL:EVAL-WHEN (:compile-toplevel :load-toplevel :execute)(CL:UNLESS (CL:FIND-PACKAGE "builtins")(make-package "builtins")(CL:USE-PACKAGE "builtins")))
;(proclaim '(optimize (speed 3)))
#| `(defgeneric __hash__ (a))`
|#
#| `(defgeneric __eq__ (a b))`
|#

(CL:DEFMETHOD |__hash__| (|a|  ) 
(CL:RETURN-FROM |__hash__| (|SXHASH| |a| )))

(CL:DEFMETHOD |__eq__| (|a| |b|  ) 
(CL:RETURN-FROM |__eq__| (|EQUALP| |a| |b| )))
(|DEFINE-HASH-TABLE-TEST| |__eq__| |__hash__| )
(CL:EXPORT '(|__eq__| |__hash__|))
(|IN-PACKAGE| "builtins" )
#|use CL
|#
(CL:IMPORT '(|CL|:|AND|))
(CL:IMPORT '(|CL|:|ASSERT|))
(CL:IMPORT '(|CL|:|BOUNDP|))
(CL:IMPORT '(|CL|:|BYTE|))
(CL:IMPORT '(|CL|:|COND|))
(CL:IMPORT '(|CL|:|COPY-SEQ|))
(CL:IMPORT '(|CL|:|DEFCLASS|))
(CL:IMPORT '(|CL|:|DEFMETHOD|))
(CL:IMPORT '(|CL|:|DEFPARAMETER|))
(CL:IMPORT '(|CL|:|DEFUN|))
(CL:IMPORT '(|CL|:|ELT|))
(CL:IMPORT '(|CL|:|EXPORT|))
(CL:IMPORT '(|CL|:|FBOUNDP|))
(CL:IMPORT '(|CL|:|FIND-CLASS|))
(CL:IMPORT '(|CL|:|FIXNUM|))
(CL:IMPORT '(|CL|:|FUNCTION|))
(CL:IMPORT '(|CL|:|GETHASH|))
(CL:IMPORT '(|CL|:|HASH-TABLE-COUNT|))
(CL:IMPORT '(|CL|:|LDB|))
(CL:IMPORT '(|CL|:|LET|))
(CL:IMPORT '(|CL|:|LENGTH|))
(CL:IMPORT '(|CL|:|LIST|))
(CL:IMPORT '(|CL|:|LOGXOR|))
(CL:IMPORT '(|CL|:|LOOP|))
(CL:IMPORT '(|CL|:|MAKE-ARRAY|))
(CL:IMPORT '(|CL|:|MAKE-HASH-TABLE|))
(CL:IMPORT '(|CL|:|MAKE-INSTANCE|))
(CL:IMPORT '(|CL|:|MOD|))
(CL:IMPORT '(|CL|:|NOT|))
(CL:IMPORT '(|CL|:|NUMBER|))
(CL:IMPORT '(|CL|:|OR|))
(CL:IMPORT '(|CL|:|QUOTE|))
(CL:IMPORT '(|CL|:|REAL|))
(CL:IMPORT '(|CL|:|RETURN-FROM|))
(CL:IMPORT '(|CL|:|ROUND|))
(CL:IMPORT '(|CL|:|SEQUENCE|))
(CL:IMPORT '(|CL|:|SETF|))
(CL:IMPORT '(|CL|:|SORT|))
(CL:IMPORT '(|CL|:|STABLE-SORT|))
(CL:IMPORT '(|CL|:|SLOT-VALUE|))
(CL:IMPORT '(|CL|:|STANDARD-OBJECT|))
(CL:IMPORT '(|CL|:|STRING|))
(CL:IMPORT '(|CL|:|SXHASH|))
(CL:IMPORT '(|CL|:|VECTOR-PUSH-EXTEND|))
(CL:IMPORT '(|CL|:|T|))
(CL:IMPORT '(|CL|:|NIL|))
(CL:IMPORT '(|CL|:*PACKAGE*))
(CL:IMPORT '(|CL|:>))
(CL:IMPORT '(|CL|:<))
(CL:IMPORT '(|CL|:*))
(CL:IMPORT '(|CL|:-))
(CL:IMPORT '(|CL|:STRING=))
(CL:IMPORT '(|SEQUENCE|:|MAKE-SEQUENCE-ITERATOR|))
(CL:IMPORT '(|CL-USER|:|__eq__|))
(CL:IMPORT '(|CL-USER|:|__hash__|))
(CL:EXPORT '(|AND| |OR|))

(cl:defmacro with-locals ((&rest names) &body body)
  (if (null names)
      (first body)
      `(symbol-macrolet ,(loop for n in names
                            collect `(,n (error '|NameError|))) ,@body)))

(cl:defmacro |muleassert| (form) `(cl:assert ,form nil '|AssertionError|))
(CL:DEFUN |hash| (|a|  ) 
(CL:RETURN-FROM |hash| (|__hash__| |a| )))

(CL:DEFMETHOD |len| (|a|  ) 
(CL:RETURN-FROM |len| (|LENGTH| |a| )))

(CL:DEFMETHOD |sorted| (|lst|  ) 
(CL:RETURN-FROM |sorted| (|SORT| (|COPY-SEQ| |lst| ) #'|__cmp__| )))

(CL:DEFMETHOD |__cmp__| (|a| |b|  ) 
(CL:RETURN-FROM |__cmp__| (< (|SXHASH| |a| ) (|SXHASH| |b| ))))

(CL:DEFMETHOD |__cmp__| ((|a| |NUMBER|) (|b| |NUMBER|)  ) 
(CL:RETURN-FROM |__cmp__| (< |a| |b|)))

(CL:DEFMETHOD |__eq__| ((|a| |REAL|) (|b| |FIXNUM|)  ) 
(CL:RETURN-FROM |__eq__| (AND (NOT (> |a| |b|) ) (NOT (< |a| |b|) ))))

(CL:DEFMETHOD |__eq__| ((|a| |FIXNUM|) (|b| |REAL|)  ) 
(CL:RETURN-FROM |__eq__| (AND (NOT (> |a| |b|) ) (NOT (< |a| |b|) ))))

(CL:DEFMETHOD |__hash__| ((|a| |FIXNUM|)  ) 
(CL:RETURN-FROM |__hash__| |a|))

(CL:DEFMETHOD |__hash__| ((|a| |REAL|)  ) 
(CL:LET ((|int_part| (|ROUND| |a| )))
(COND ((|__eq__| |int_part| |a|) (CL:RETURN-FROM |__hash__| |int_part|)))
(CL:RETURN-FROM |__hash__| (|SXHASH| |a| ))))
(CL:DEFCLASS |object| (|STANDARD-OBJECT|) NIL) 
(CL:DEFMETHOD |__repr__| ((|self| |object|)   ) 
(CL:RETURN-FROM |__repr__| "")) (CL:DEFUN |object| (  ) 
(CL:LET ((|self| (CL:MAKE-INSTANCE '|object|)))
|self|))
(cl:declaim (cl:inline |setitem|))
(CL:DEFCLASS |dict| (|SEQUENCE| |object|) (|hash_table|)) 
(CL:DEFMETHOD |len| ((|self| |dict|)   ) 
(CL:RETURN-FROM |len| (|HASH-TABLE-COUNT| (CL:SLOT-VALUE |self| '|hash_table|) ))) 
(CL:DEFMETHOD |keys| ((|self| |dict|)   ) 
(CL:LET ((|hashtable| (CL:SLOT-VALUE |self| '|hash_table|)))
(CL:LET ((|lst| (|list|  )))
(loop for key being the hash-keys of |hashtable|
             do (|append| |lst| key))
(CL:RETURN-FROM |keys| |lst|)))) 
(CL:DEFMETHOD |__eq__| ((|self| |dict|) |other|  ) 
(CL:LET ((|keys| (|sort| (|keys| |self| ) )))
(COND ((CL:NOT (|__eq__| |keys| (|sort| (|keys| |other| ) ))) (CL:RETURN-FROM |__eq__| nil)))
(CL:LOOP FOR |key| BEING THE ELEMENTS OF |keys| DO (COND ((CL:NOT (|__eq__| (|getitem| |self| |key|) (|getitem| |other| |key|))) (CL:RETURN-FROM |__eq__| nil))))
(CL:RETURN-FROM |__eq__| t))) 
(CL:DEFMETHOD |setitem| ((|self| |dict|) |key| |value|  ) 
(|SETF| (|GETHASH| |key| (CL:SLOT-VALUE |self| '|hash_table|) ) |value| )) 
(CL:DEFMETHOD |getitem| ((|self| |dict|) |key|  ) 
(CL:MULTIPLE-VALUE-BIND (|value| |found|) (|GETHASH| |key| (CL:SLOT-VALUE |self| '|hash_table|) ) (COND ((NOT |found| ) (CL:ERROR '|KeyError| )))
(CL:RETURN-FROM |getitem| |value|))) (CL:DEFUN |dict| (  CL:&REST |args|) 
(CL:LET ((|self| (CL:MAKE-INSTANCE '|dict|)))
(CL:SETF (CL:SLOT-VALUE |self| '|hash_table|) (|MAKE-HASH-TABLE|  :|TEST| (|QUOTE| |__eq__| )))
|self|))
(CL:DEFCLASS |list| (|SEQUENCE| |object|) (|array|)) 
(CL:DEFMETHOD |__eq__| ((|self| |list|) |other|  ) 
(COND ((CL:NOT (|__eq__| (|len| |self| ) (|len| |other| ))) (CL:RETURN-FROM |__eq__| nil)))
(CL:LOOP FOR |i| FROM 0 BELOW (|len| |self| ) BY 1 DO (COND ((CL:NOT (|__eq__| (|getitem| |self| |i|) (|getitem| |other| |i|))) (CL:RETURN-FROM |__eq__| nil))))
(CL:RETURN-FROM |__eq__| t)) 
(CL:DEFMETHOD |__hash__| ((|self| |list|)   ) 
(CL:RETURN-FROM |__hash__| 1)) 
(CL:DEFMETHOD |sort| ((|self| |list|)  CL:&KEY (|order| (|FUNCTION| |__cmp__| )) ) 
(|STABLE-SORT| (CL:SLOT-VALUE |self| '|array|) |order| )) 
(CL:DEFMETHOD |sorted| ((|self| |list|)   ) 
(CL:RETURN-FROM |sorted| (|list| (|SORT| (|COPY-SEQ| (CL:SLOT-VALUE |self| '|array|) ) (|FUNCTION| |__cmp__| ) ) ))) 
(CL:DEFMETHOD |len| ((|self| |list|)   ) 
(CL:RETURN-FROM |len| (|LENGTH| (CL:SLOT-VALUE |self| '|array|) ))) 
(CL:DEFMETHOD |setitem| ((|self| |list|) |key| |value|  ) 
(|SETF| (|ELT| (CL:SLOT-VALUE |self| '|array|) |key| ) |value| )) 
(CL:DEFMETHOD |getitem| ((|self| |list|) |key|  ) 
(CL:RETURN-FROM |getitem| (|ELT| (CL:SLOT-VALUE |self| '|array|) |key| ))) 
(CL:DEFMETHOD |append| ((|self| |list|) |val|  ) 
(|VECTOR-PUSH-EXTEND| |val| (CL:SLOT-VALUE |self| '|array|) )) 
(CL:DEFMETHOD SEQUENCE:LENGTH ((|self| |list|)   ) 
(CL:RETURN-FROM SEQUENCE:LENGTH (|LENGTH| (CL:SLOT-VALUE |self| '|array|) ))) 
(CL:DEFMETHOD SEQUENCE:ELT ((|self| |list|) |index|  ) 
(CL:RETURN-FROM SEQUENCE:ELT (|getitem| |self| |index| ))) 
(CL:DEFMETHOD |MAKE-SEQUENCE-ITERATOR| ((|self| |list|)  CL:&KEY (|FROM-END| NIL)(|START| 0)(|END| NIL) ) 
(CL:RETURN-FROM |MAKE-SEQUENCE-ITERATOR| (|MAKE-SEQUENCE-ITERATOR| (CL:SLOT-VALUE |self| '|array|) :|FROM-END| |FROM-END| :|START| |START| :|END| |END|))) (CL:DEFUN |list| (  CL:&REST |args|) 
(CL:LET ((|self| (CL:MAKE-INSTANCE '|list|)))
(CL:SETF (CL:SLOT-VALUE |self| '|array|) (|MAKE-ARRAY| 0 :|ADJUSTABLE| t :|FILL-POINTER| t))
(CL:LOOP FOR |lst| BEING THE ELEMENTS OF |args| DO (CL:LOOP FOR |x| BEING THE ELEMENTS OF |lst| DO (|append| |self| |x| )))
|self|))
(CL:DEFCLASS |tuple| (|list|) (|array|)) 
(CL:DEFMETHOD |__hash__| ((|self| |tuple|)   ) 
(CL:LET ((|value| #x345678))
(CL:LOOP FOR |item| BEING THE ELEMENTS OF (CL:SLOT-VALUE |self| '|array|) DO (CL:PROGN (CL:SETF |value| (MOD |value| 295147905179352825856))
(CL:SETF |value| (LOGXOR (MOD (* 1000003 |value|) 295147905179352825856) (|__hash__| |item| ))))
)
(CL:SETF |value| (LOGXOR |value| (|len| |self| )))
(COND ((|__eq__| |value| (- 1 )) (CL:SETF |value| (- 2 ))))
(CL:SETF |value| (MOD |value| 295147905179352825856))
(CL:RETURN-FROM |__hash__| (|LDB| (|BYTE| 62 0 ) |value| )))) 
(CL:DEFMETHOD |setitem| ((|self| |tuple|) |key| |value|  ) 
(|SETF| (|ELT| (CL:SLOT-VALUE |self| '|array|) |key| ) |value| )) 
(CL:DEFMETHOD |getitem| ((|self| |tuple|) |key|  ) 
(CL:RETURN-FROM |getitem| (|ELT| (CL:SLOT-VALUE |self| '|array|) |key| ))) 
(CL:DEFMETHOD |append| ((|self| |tuple|) |val|  ) 
(|VECTOR-PUSH-EXTEND| |val| (CL:SLOT-VALUE |self| '|array|) )) (CL:DEFUN |tuple| (  CL:&REST |args|) 
(CL:LET ((|self| (CL:MAKE-INSTANCE '|tuple|)))
(CL:SETF (CL:SLOT-VALUE |self| '|array|) (|MAKE-ARRAY| 0 :|ADJUSTABLE| t :|FILL-POINTER| t))
(CL:LOOP FOR |lst| BEING THE ELEMENTS OF |args| DO (CL:LOOP FOR |x| BEING THE ELEMENTS OF |lst| DO (|append| |self| |x| )))
|self|))

(CL:DEFMETHOD |getitem| ((|s| |STRING|) |index|  ) 
(CL:RETURN-FROM |getitem| (|ELT| |s| |index| )))

(CL:DEFMETHOD |setitem| ((|s| |STRING|) |key| |value|  ) 
(CL:ERROR '|TypeError| ))

(CL:DEFMETHOD |__eq__| ((|s| |STRING|) (|other| |STRING|)  ) 
(CL:RETURN-FROM |__eq__| (STRING= |s| |other| )))

(CL:DEFMETHOD |__cmp__| ((|s| |STRING|) (|other| |STRING|)  ) 
(CL:RETURN-FROM |__cmp__| (STRING< |s| |other| )))
(CL:LOOP FOR S BEING EACH PRESENT-SYMBOL IN CL:*PACKAGE*
   WHEN (OR (CL:FBOUNDP S)
            (CL:BOUNDP S)
            (CL:FIND-CLASS S NIL))
   DO (CL:EXPORT S))
