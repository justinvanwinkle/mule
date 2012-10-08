(progn
  (defclass clpython.user::|Test| ()
    ())
  (defclass clpython.user::|SubTest| (Test)
    ())
  (defmethod clpython.user::|foo| (self clpython.user::|SubTest|)
    (let ((clpython.user::|x| 5))
      (print clpython.user::|x|)))
  (defun clpython.user::|not_method| ()
         (block nil
           nil)))
