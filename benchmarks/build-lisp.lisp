;;; Run: /usr/bin/sbcl --noinform --no-sysinit --no-userinit --script benchmarks/build-lisp.lisp
(let* ((directory (make-pathname :name nil :type nil :defaults *load-truename*))
       (source (merge-pathnames "workload.lisp" directory))
       (output-directory (merge-pathnames "../build/benchmarks/" directory))
       (fasl (merge-pathnames "workload.fasl" output-directory))
       (executable (merge-pathnames "workload-sbcl" output-directory)))
  (ensure-directories-exist executable)
  (multiple-value-bind (compiled warnings failure) (compile-file source :output-file fasl)
    (declare (ignore warnings))
    (when failure
      (error "SBCL failed to compile ~A" source))
    (load compiled))
  (sb-ext:save-lisp-and-die
   executable
   :toplevel (symbol-function (find-symbol "MAIN" "MULE-BENCHMARK"))
   :executable t))
