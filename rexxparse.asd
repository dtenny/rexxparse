(in-package :cl-user)

(defpackage :rexxparse-asd
  (:use :cl :asdf))

(in-package :rexxparse-asd)

(defsystem :rexxparse
  :version "0.1.0"
  :license "MIT"
  :author "Dave Tenny"
  :description "A trivial parsing tool inspired by the REXX PARSE construct."
  ;;:bug-tracker "https://github.com/dtenny/rexxparse/issues"
  ;;:source-control (:git "https://github.com/dtenny/rexxparse")
  :depends-on (:alexandria :parse-float)
  :serial t
  :components ((:file "package")
               (:file "rexxparse")))
