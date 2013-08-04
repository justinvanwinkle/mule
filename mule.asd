(defpackage "mule-system" (:use :cl))
(in-package "mule-system")
(asdf:defsystem "mule"
  :serial t
  :version "0.0.1"
  :description "An experiment in syntax and langauge extension"
  :author "Justin Van Winkle <justin.vanwinkle@gmail.com>"
  :license "GPLv3"
  :depends-on (:asdf)
  :components ((:module "build"
                         :components
                         ((:file "lib/builtins")))))
