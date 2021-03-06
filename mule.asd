(defpackage "mule" (:use :cl))
(in-package "mule")
(asdf:defsystem "mule"
  :serial t
  :version "0.0.1"
  :description "An experiment in syntax and langauge extension"
  :author "Justin Van Winkle <justin.vanwinkle@gmail.com>"
  :license "GPLv3"
  :depends-on (:asdf)
  :components ((:file "build/lib/builtins")
               (:file "build/lib/mload")))
