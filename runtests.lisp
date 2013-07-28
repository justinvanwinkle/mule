

(defun read-file (filespec)
  (with-open-file (source-file filespec)
    (loop for form = (read source-file nil :eof)
         until (eq form :eof)
         collect form)))

(defun compare-files (fn1 fn2)
  (let ((f1 (read-file fn1))
        (f2 (read-file fn2)))
    (if (equalp f1 f2)
         (format t "MATCHES ~A ~%" (pathname-name fn1))
         (progn
           (format t "FAILS ~A ~%" (pathname-name fn1))
           (pprint f1)
           (print "**")
           (pprint f2)
           (print "**")))))


(dolist (fn (directory "test_cases/*.lisp"))
  (let ((fn1 (make-pathname :directory (pathname-directory fn)
                            :name (pathname-name fn)
                            :type "lisp"))
        (fn2 (make-pathname :directory '(:relative "OUTPUT")
                            :name (pathname-name fn)
                            :type "lisp")))
    (compare-files fn1 fn2)))
