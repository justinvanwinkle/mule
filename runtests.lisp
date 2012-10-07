(load "compile.lisp")

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(dolist (fn (directory "test_cases/*.py"))
  (let ((compiled (pycl-compile fn))
        (lispcode (read-from-string (file-string
                                     (make-pathname :directory (pathname-directory fn)
                                                    :name (pathname-name fn)
                                                    :type "lisp")))))
    (if (equal compiled lispcode)
        (format t "++++~a has passed~%" fn)
        (format t "----~a has failed~%~a~%~%~a~%" fn compiled lispcode))))
