(in-package :mule-system (:use :cl))
(asdf:defsystem :mule
  :serial t
  :description "An experiment in syntax and langauge extension"
  :author "Justin Van Winkle <justin.vanwinkle@gmail.com>"
  :license "Specify license here"
  :depends-on (:cl :asdf)
  :components ((:file "package") (:file "hello-world-ltk")))
