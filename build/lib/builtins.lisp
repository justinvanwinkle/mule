
(EVAL-WHEN (:COMPILE-TOPLEVEL)
  (SETF *READTABLE* (COPY-READTABLE NIL))
  (SETF (READTABLE-CASE *READTABLE*) :PRESERVE))
(DEFPACKAGE "builtins"
  (:USE "CL" "SB-EXT"))
(IN-PACKAGE "builtins")
(DEFCLASS Dictionary NIL (hash_table))
(DEFMETHOD init ((self Dictionary))
  (SETF (SLOT-VALUE self 'hash_table) (MAKE-HASH-TABLE :TEST 'EQUALP)))
(DEFMETHOD setitem ((self Dictionary) key value)
  (SETF (GETHASH key (SLOT-VALUE self 'hash_table)) value))
(DEFMETHOD getitem ((self Dictionary) key)
  (RETURN-FROM getitem (GETHASH key (SLOT-VALUE self 'hash_table))))
(DEFUN Dictionary ()
  (LET ((self (MAKE-INSTANCE 'Dictionary)))
    (init self)
    self))
(DEFCLASS List (SEQUENCE STANDARD-OBJECT) (array))
(DEFMETHOD init ((self List))
  (SETF (SLOT-VALUE self 'array) (MAKE-ARRAY 0 :ADJUSTABLE T :FILL-POINTER T)))
(DEFMETHOD setitem ((self List) key value)
  (SETF (SLOT-VALUE self 'array) value))
(DEFMETHOD getitem ((self List) key) (AREF (SLOT-VALUE self 'array) key))
(DEFMETHOD append ((self List) val)
  (VECTOR-PUSH-EXTEND val (SLOT-VALUE self 'array)))
(DEFMETHOD SB-SEQUENCE:LENGTH ((self List))
  (RETURN-FROM SB-SEQUENCE:LENGTH (LENGTH (SLOT-VALUE self 'array))))
(DEFMETHOD SB-SEQUENCE:ELT ((self List) index)
  (RETURN-FROM SB-SEQUENCE:ELT (getitem self index)))
(DEFMETHOD MAKE-SEQUENCE-ITERATOR
           ((self List) &KEY (FROM-END NIL) (START NIL) (END NIL))
  (RETURN-FROM MAKE-SEQUENCE-ITERATOR
    (MAKE-SEQUENCE-ITERATOR (SLOT-VALUE self 'array) :FROM-END FROM-END :START
     START :END END)))
(DEFUN List ()
  (LET ((self (MAKE-INSTANCE 'List)))
    (init self)
    self))
