(load "build/lib/builtins/builtins.lisp")
(load "build/lib/builtins/mload.lisp")
(load "build/lib/builtins/exceptions.lisp")
(use-package "builtins")

(print (directory (make-pathname
                            :directory '(:relative "build" "lib")
                            :name :wild
                            :type "lisp")))
(dolist (lib_fn (directory (make-pathname
                            :directory '(:relative "build" "lib")
                            :name :wild
                            :type "lisp")))
  (format t "loading ~A~%" lib_fn)
  (|mload| lib_fn))
(save-lisp-and-die "mule.core")
