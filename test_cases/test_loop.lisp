(defun loopy_loop ()
  (loop for x in ' (1 2 3)
  do (print x)
    (print (+ x x))))
(loopy_loop)

