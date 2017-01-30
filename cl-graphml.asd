(in-package :cl-user)
(defpackage cl-graphml-asd
  (:use :cl :asdf))
(in-package :cl-graphml-asd)

(defsystem cl-graphml
  :name "cl-graphml"
  :description "A simple experiemntal library writen in common lisp to interact with GraphML files"
  :depends-on (:plump
	       :alexandria)
  :components ((:file "cl-graphml")))
