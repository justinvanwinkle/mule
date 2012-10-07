(progn
  (defun clpython.user::|test| ()
    (block nil
      (let ((clpython.user::|x| 1))
        (let ((clpython.user::|y| 2))
          (+ clpython.user::|x| clpython.user::|y|))))))
