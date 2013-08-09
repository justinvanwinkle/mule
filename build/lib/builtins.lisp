(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "builtins")(make-package "builtins" :use '("COMMON-LISP"))) ) (in-package "builtins")
(cl:require 'asdf)
(cl:if (cl:not (cl:equal (cl:package-name cl:*package*) "builtins"))
  (asdf:load-system :mule))
(use-package "builtins")(DEFCLASS |dict| () (|hash_table|)) (DEFMETHOD |init| ((|self| |dict|) |val|  ) (SETF (SLOT-VALUE |self| '|hash_table|) (MAKE-HASH-TABLE :TEST 'EQUALP))) (DEFMETHOD |setitem| ((|self| |dict|) |key| |value|  ) (SETF (GETHASH |key| (SLOT-VALUE |self| '|hash_table|)) |value|)) (DEFMETHOD |getitem| ((|self| |dict|) |key|  ) (RETURN-FROM |getitem| (GETHASH |key| (SLOT-VALUE |self| '|hash_table|))))  (DEFCLASS |list| (|SEQUENCE| STANDARD-OBJECT) (|array|)) (DEFMETHOD |init| ((|self| |list|) |val|  ) (SETF (SLOT-VALUE |self| '|array|) (MAKE-ARRAY 0 :ADJUSTABLE T :FILL-POINTER T))) (DEFMETHOD |setitem| ((|self| |list|) |key| |value|  ) (SETF  (SLOT-VALUE |self| 'array) |value|)) (DEFMETHOD |getitem| ((|self| |list|) |key|  ) (AREF (SLOT-VALUE |self| '|array|) |key|)) (DEFMETHOD |append| ((|self| |list|) |val|  ) (VECTOR-PUSH-EXTEND |val| (SLOT-VALUE |self| '|array|))) (DEFMETHOD SEQUENCE:LENGTH ((|self| |list|)   ) (RETURN-FROM SEQUENCE:LENGTH (|LENGTH| (SLOT-VALUE |self| '|array|) ))) (DEFMETHOD SEQUENCE:ELT ((|self| |list|) |index|  ) (RETURN-FROM SEQUENCE:ELT (|getitem| |self| |index| ))) (DEFMETHOD MAKE-SEQUENCE-ITERATOR ((|self| |list|)  &KEY (FROM-END NIL)(|START| NIL)(|END| NIL) ) (RETURN-FROM MAKE-SEQUENCE-ITERATOR (MAKE-SEQUENCE-ITERATOR (SLOT-VALUE |self| '|array|) :FROM-END FROM-END :|START| |START| :|END| |END|)))  (DEFCLASS |tuple| (|SEQUENCE| STANDARD-OBJECT) (|array|)) (DEFMETHOD |init| ((|self| |tuple|) |val|  ) NIL) 
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

