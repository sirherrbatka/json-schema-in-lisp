(in-package #:cl-user)

(asdf:defsystem json-schema-in-lisp
  :name "lson"
  :version "0.0.0"
  :license "MIT"
  :author "Marek Kochanowicz"
  :maintainer "Marek Kochanowicz"
  :depends-on (:iterate
               :alexandria
               :serapeum
               :cl-annot
               :closer-mop
               :cl-json)
  :serial T
  :components ((:file "src/package")
               (:file "src/macros")
               (:file "src/variables")
               (:file "src/parser")
               (:file "src/mop")
               (:file "src/map")
               (:file "src/construct")))
