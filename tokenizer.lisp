(ql:quickload "cl-ppcre")

(defclass token ()
  ((text :initarg :text
         :initform nil
         :accessor token-text)
   (type :initarg :type
         :initform nil
         :accessor token-type)))


(defmethod print-object ((token-obj token) stream)
  (with-slots (text type) token-obj
    (format stream "#<TOKEN :text \"~A\" :type ~A>" text type)))


(defun populate-tokens (table)
    (loop for (regex type) in table
         collect (list (concatenate 'string "^" regex)
                       ;; the loop macro's destructuring bind doesn't
                       ;; create a real lexical scope
                       (let ((type type))
                         (when type
                           (lambda (match)
                             (make-instance 'token
                                            :text match
                                            :type type)))))))


(defvar *tokens* (populate-tokens
                  '(("(\\.?[0-9]+)+" 'NUMBER)
                    ("[_a-zA-Z][_a-zA-Z0-9]*" 'NAME)
                    ("==" 'EQUALS)
                    ("=" 'ASSIGNMENT)
                    ("\\+" 'PLUS)
                    ("-" 'MINUS)
                    ("\\*" 'MULTIPLY)
                    ("/" 'DIVIDE)
                    ("\\(" 'LEFT-PAREN)
                    ("\\)" 'RIGHT-PAREN)
                    (" " nil))))


(defun match (regex token-init input)
  (multiple-value-bind (start end) (cl-ppcre:scan regex input)
    (when (and start (zerop start))
      (values end (when token-init
                    (funcall token-init (subseq input start end)))))))


(defun tokenize (tokens input)
  (remove-if
   #'not
   (loop collect
       (dolist (regex-token-init tokens (error "couldn't tokenize"))
         (destructuring-bind (regex token-init) regex-token-init
           (multiple-value-bind (end token-obj) (match regex token-init input)
             (when end
               (setf input (subseq input end))
               (return token-obj)))))
      until (zerop (length input)))))


(defclass parse-tables ()
  ((infix-parselets)))


(defclass parser ()
  ((tokens :initarg :tokens
           :initform nil
           :accessor parser-tokens)
   (current-token :initform nil)
   (ast :initform nil
        :accessor parser-ast)))


(defgeneric parser-consume-token (p)
  (:method ((p parser))
    (with-slots (current-token tokens) p
      (setf current-token (car tokens))
      (setf tokens (cdr tokens))
      current-token)))


(defgeneric parser-parse (p)
  (:method ((p parser))
    (with-slots (ast) p
      (let* ((current-token (parser-consume-token p))
             (prefix ()))
        (unless current-token
          (return nil))))))
