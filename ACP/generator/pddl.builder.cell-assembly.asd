
(defsystem pddl.builder.cell-assembly
  :version "0.1"
  :author "guicho"
  :license "LLGPL"
  :depends-on (:aflab1
	       :guicho-utilities
               :iterate
               :cl-ppcre
               :osicat
               :alexandria)
  :serial t
  :components ((:file :make-distances)
               (:file :model2a)
               (:file :model2b)
               (:file :model3a)
               (:file :model3b)
               (:file :model3c))
  :description "PDDL writer (modified specifically for cell-assembly)")
