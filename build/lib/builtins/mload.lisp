(eval-when (:compile-toplevel :load-toplevel :execute)(unless (find-package "mload")(make-package "mload" :use '("COMMON-LISP"))(use-package "builtins")))
;(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(|REQUIRE| "SB-MD5" )
(DEFUN |mload| (|fn| &KEY (|cache| t) ) (COND ((|PROBE-FILE| |fn| ) (LET ((|md5| (|FORMAT| NIL "~(~{~2,'0X~}~)" (|COERCE| (SB-MD5:MD5SUM-FILE |fn| ) (|QUOTE| |LIST| ) ) ))) (LET ((|cache_fn| (|MAKE-PATHNAME|  :|DIRECTORY| '(:absolute "tmp") :|NAME| |md5| :|TYPE| "fasl"))) (COND (|cache| (COND ((|PROBE-FILE| |cache_fn| ) (|LOAD| |cache_fn| )) (t (|COMPILE-FILE| |fn| :|OUTPUT-FILE| |cache_fn|))))))))))
(LOOP FOR S BEING EACH PRESENT-SYMBOL IN *PACKAGE*
   WHEN (OR (FBOUNDP S) (BOUNDP S) (FIND-CLASS S NIL))
   DO (EXPORT S))

