

(defun read-file (filespec)
  (with-open-file (source-file filespec)
    (loop collect (read source-file))))

(defun compare-files (fn1 fn2)
  (let ((f1 (read-file fn1))
        (f2 (read-file fn2)))
    (if (equalp f1 f2)
         (format nil "~A MATCHES ~A" fn1 fn2)
         (progn
           (format nil "~A FAIL ~A" fn1 fn2)
           (pprint f1)
           (print "**")
           (pprint f2)))))


(dolist (fn (directory "test_cases/*.lisp"))
  (let ((fn1 (make-pathname :directory (pathname-directory fn)
                            :name (pathname-name fn)
                            :type "lisp"))
        (fn2 (make-pathname :directory "OUTPUT/"
                            :name (pathname-name fn))))
    (compare-files fn1 fns)))
