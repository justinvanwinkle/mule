;;;; Parse pipe-delimited key-value records and format normalized summaries.

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load #.(merge-pathnames "perfutils/case.lisp"
                           (or *compile-file-truename* *load-truename*))
        :verbose nil :print nil))

(defparameter *name* "kv_record_parse")
(defparameter *description* "Delimited key-value string splitting, partitioning, and formatting")
(defparameter *default-size* 50000)
(defparameter *default-seed* 42)

(defparameter *records*
  #("svc=auth|region=us-east|level=info|ms=12|code=200"
    "svc=billing|region=eu-west|level=warn|ms=85|code=429"
    "svc=search|region=ap-south|level=error|ms=240|code=503"
    "svc=cache|region=us-west|level=info|ms=4|code=200"
    "svc=gateway|region=us-east|level=warn|ms=63|code=502"
    "svc=storage|region=eu-central|level=info|ms=19|code=200"))

(defun split-char (text ch)
  (declare (type simple-string text)
           (type character ch))
  (let ((start 0)
        (parts nil))
    (declare (type fixnum start))
    (loop
      (let ((pos (position ch text :start start)))
        (if pos
            (progn
              (push (subseq text start pos) parts)
              (setf start (+ pos 1)))
            (progn
              (push (subseq text start) parts)
              (return (nreverse parts))))))))

(defun partition-char (text ch)
  (declare (type simple-string text)
           (type character ch))
  (let ((pos (position ch text)))
    (if pos
        (values (subseq text 0 pos)
                (string ch)
                (subseq text (+ pos 1)))
        (values text "" ""))))

(defun prepare (size seed)
  (declare (type fixnum size seed))
  (when (< size 0)
    (error "size must be nonnegative"))
  (let ((state (logand seed #x7FFFFFFF))
        (records (make-array size))
        (templates *records*)
        (n-templates (length *records*)))
    (declare (type fixnum state n-templates)
             (type simple-vector records templates))
    (dotimes (i size records)
      (declare (type fixnum i))
      (setf state (logand (+ (* state 1103515245) 12345) #x7FFFFFFF))
      (let ((base (the simple-string (svref templates (mod (ash state -8) n-templates))))
            (seq (write-to-string (+ (mod (ash state -16) 1000) (logand i 15)))))
        (setf (svref records i)
              (concatenate 'string base "|seq=" seq))))))

(defun run-kernel (data)
  (declare (type simple-vector data))
  (let* ((n (length data))
         (rounds (max 1 (floor n 400)))
         (total-ms 0)
         (alerts 0)
         (summary-bytes 0))
    (declare (type fixnum n rounds total-ms alerts summary-bytes))
    (dotimes (round-idx rounds)
      (declare (type fixnum round-idx))
      (let ((mask (+ (logand round-idx 7) 1)))
        (declare (type fixnum mask))
        (dotimes (i n)
          (declare (type fixnum i))
          (let ((rec (the simple-string (svref data i)))
                (svc "")
                (level "")
                (ms 0)
                (seq 0))
            (declare (type simple-string rec svc level)
                     (type fixnum ms seq))
            (dolist (kv (split-char rec #\|))
              (multiple-value-bind (key sep val)
                  (partition-char (the simple-string kv) #\=)
                (declare (ignore sep)
                         (type simple-string key val))
                (cond
                  ((string= key "svc") (setf svc val))
                  ((string= key "level") (setf level val))
                  ((string= key "ms") (setf ms (the fixnum (parse-integer val))))
                  ((string= key "seq") (setf seq (the fixnum (parse-integer val)))))))
            (when (and (> (length level) 0)
                       (let ((c (schar level 0)))
                         (or (char= c #\w) (char= c #\e))))
              (incf alerts))
            (incf total-ms (the fixnum (+ ms (logand seq mask))))
            (let ((tag (concatenate 'string svc ":" level)))
              (incf summary-bytes (length tag)))))))
    (list total-ms alerts summary-bytes)))

(defun main ()
  (run-case *name* *description* *default-size* *default-seed* #'prepare #'run-kernel))

(eval-when (:execute)
  (main))
