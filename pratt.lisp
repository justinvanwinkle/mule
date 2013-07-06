(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))
(ql:quickload "cl-ppcre")

(defstruct string-state
  in-string
  string-start-char)


(defun parse (source-string string-state comment-state tokenizer parser)
  (map nil (
