

(defun read-file (filespec)
  (with-open-file (source-file filespec)
    (let ((*readtable* (copy-readtable nil)))
      (setf (readtable-case *readtable*) :preserve)
      (loop for form = (read source-file nil :eof)
         until (eq form :eof)
         collect form))))

(defun pprint-forms (forms)
  (loop for form in forms
       do (pprint form)))

(defun compare-files (fn1 fn2)
  (let ((f1 (read-file fn1))
        (f2 (read-file fn2)))
    (if (equalp f1 f2)
         (format t "MATCHES ~A ~%" (pathname-name fn1))
         (let ((*readtable* (copy-readtable nil)))
           (setf (readtable-case *readtable*) :preserve)
           (format t "FAILS ~A ~%" (pathname-name fn1))
           (pprint-forms f1)
           (print "**")
           (pprint-forms f2)
           (print "**")))))


(dolist (fn (directory "test_cases/*.lisp"))
  (let ((fn1 (make-pathname :directory (pathname-directory fn)
                            :name (pathname-name fn)
                            :type "lisp"))
        (fn2 (make-pathname :directory '(:relative "OUTPUT")
                            :name (pathname-name fn)
                            :type "lisp")))
    (compare-files fn1 fn2)))
