(proclaim '(optimize (safety 3) (speed 0)))

(defpackage "IR"
  (:use "COMMON-LISP"))

(in-package IR)

(defun elements-are-type (type)
  (flet ((f (seq) (every #'(lambda (x) (typep x type)) seq)))
    #'f))

(deftype ir-type-list ()
  '(and list (satisfies (elements-are-ir-type 'ir-type))))

(deftype typed-name-list ()
  '(and list (satisfies (elements-are-ir-type 'typed-name))))

(deftype non-negative-fixnum ()
  `(integer 0 , most-positive-fixnum))

(defclass module () (parts))

(defclass define ()
  ((return-type :type ir-type
                :initarg :type)
   (name :type identifier
         :initarg :name)))

(defclass identifier () (name))
(defclass global-identifier (identifier)
  ((start-char :initform "@")))
(defclass local-identifier (identifier)
  ((start-char :initform "%")))


(defclass ir-type () ())
(defclass integer-type (ir-type)
  ((bits :type non-negative-fixnum
         :initarg :bits)))

(defparameter i32 (make-instance 'integer-type :bits 32))
(defparameter i8 (make-instance 'integer-type :bits 8))

(defclass base-float-type (ir-type) ())

(defclass half-float-type (base-float-type) ())
(defclass float-type (base-float-type) ())
(defclass double-type (base-float-type) ())
(defclass fp128-type (base-float-type) ())
(defclass x86-fp80-type (base-float-type) ())
(defclass ppc-fp128-type (base-float-type) ())

(defclass array-type (ir-type)
  ((size :type non-negative-fixnum
         :initarg :size)
   (of :type ir-type
         :initarg :of)))

(defclass function-type (ir-type)
  ((return-type :type ir-type
                :initarg :return-type)
   (parameter-list :type ir-type-list
                   :initarg :parameter-list)))

(defclass pointer-type (ir-type)
  ((point-to :type ir-type
             :initarg :point-to)))

(defclass struct-type (ir-type)
  ((entries :type ir-type-list
            :initarg :entries)))

(defclass packed-struct-type (struct-type) ())

(defclass vector-type (array-type) ())

(defclass instruction ()
  ((op1 :initarg :op1)
   (op2 :initarg :op2)
   (flags :initarg :flags)))



(defun instruction-slot (slot-name)
  (list (intern (symbol-name slot-name)) :initarg (intern (symbol-name slot-name) :keyword)))

(instruction-slot 'derp)

(defmacro definstruction (instruction-symbol &rest rest) `(defclass ,instruction-symbol

(defclass typed-name ()
  ((type :type ir-type
         :initarg :type)
   (name :type identifier
         :initarg name)))

(defclass define ()
  ((name :type typed-name
         :initarg :name)
   (name-counter :type hash-table
                 :initform (make-hash-table :test 'equal))
   (params :type typed-name-list
           :initarg :params)
   (tagblock :type tagblock-list
             :initarg :tagblock)))

(defclass label ()
  ((name :type string
         :initarg name)))

(defclass labelbody ()
  ((label :type label
          :initarg :label)
   (ops :type op-list
        :initarg :ops)))

(defmethod dump-ir ((a integer-type))
  (with-slots (bits) a
    (format nil "i~A" bits)))

(defmethod dump-ir ((f base-float-type))
  (dolist (pair '((half-float-type "half")
                  (float-type "float")
                  (double-type "double")
                  (fp128-type "fp128")
                  (x86-fp80-type "x86_fp80")
                  (ppc-fp128-type "ppc_fp128")))
    (print (first pair))
    (when (typep f (first pair))
      (return-from dump-ir (second pair)))))


(defmethod dump-ir ((a array-type))
  (with-slots (size of) a
    (format nil "[~a x ~a]" size (dump-ir of))))

(defmethod dump-ir ((a function-type))
  (with-slots (return-type parameter-list) a
    (format nil "~a ~a" (dump-ir return-type) (mapcar #'dump-ir parameter-list))))

(defmethod dump-ir ((p pointer-type))
    (with-slots (point-to) p
      (format nil "~a *" (dump-ir point-to))))

(defmethod dump-ir ((s struct-type))
    (with-slots (entries) s
      (format nil "{~{~a~^, ~}}" (mapcar #'dump-ir entries))))

(defmethod dump-ir ((v vector-type))
  (with-slots (size of) v
    (format nil "<~a x ~a>" size (dump-ir of))))

(defmethod dump-ir ((s packed-struct-type))
    (format nil "<~A>" (call-next-method)))


"
define i32 @mul_add(i32 %x, i32 %y, i32 %z) {
entry:
  %tmp = mul i32 %x, %y
  %tmp2 = add i32 %tmp, %z
  ret i32 %tmp2
}
"
