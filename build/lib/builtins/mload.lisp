(CL:EVAL-WHEN (:compile-toplevel :load-toplevel :execute)(CL:UNLESS (CL:FIND-PACKAGE "mload")(make-package "mload")(CL:USE-PACKAGE "builtins")))
(proclaim '(optimize (space 0) (safety 0) (speed 3)))
(|REQUIRE| "SB-MD5" )
(CL:DEFUN |mload| (|fn| CL:&KEY (|cache| t) ) 
(COND ((|PROBE-FILE| |fn| ) (CL:LET ((|md5| (|FORMAT| NIL "~(~{~2,'0X~}~)" (|COERCE| (SB-MD5:MD5SUM-FILE |fn| ) (|QUOTE| |LIST| ) ) )))
(CL:LET ((|cache_fn| (|MAKE-PATHNAME|  :|DIRECTORY| '(:absolute "tmp") :|NAME| |md5| :|TYPE| "fasl")))
(COND (|cache| (COND ((|PROBE-FILE| |cache_fn| ) (|LOAD| |cache_fn| )) (t (|COMPILE-FILE| |fn| :|OUTPUT-FILE| |cache_fn|))))))))))
(CL:LOOP FOR S BEING EACH PRESENT-SYMBOL IN CL:*PACKAGE*
   WHEN (OR (CL:FBOUNDP S) (CL:BOUNDP S) (CL:FIND-CLASS S NIL))
   DO (CL:EXPORT S))

