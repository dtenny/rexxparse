(in-package :cl-user)

(defpackage :rexxparse-test-asd
  (:use :cl :asdf))

(in-package :rexxparse-test-asd)

(defsystem :rexxparse-test
  :version "0.1.1"
  :license "MIT"
  :author "Dave Tenny"
  :description "Tests for the :rexxparse package."
  :depends-on (:rexxparse :fiveam)
  :components ((:file "test")))
