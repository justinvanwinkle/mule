(dolist (lib_fn (directory (make-pathname
                            :directory '(:relative "build" "lib")
                            :name :wild
                            :type "lisp")))
  (load lib_fn))
(save-lisp-and-die "mule.core")
