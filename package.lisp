(in-package :cl-user)

(defpackage :rexxparse
  (:use :cl)

  (:export 

   ;; The only thing you need most of the time.
   #:parse
   ;; To change the default values of unmatched PARSE variables.
   ;; Defaults to the empty string.
   #:*unmatched-binding-value*
   ;; To specify what kind of scanner you'd like to use for pattern literals
   ;; if REXXPARSE doesn't give you the behavior you want.
   #:*pattern->scanner*
   ;; In case you want to call this from your custom scanner for pattern
   ;; literals you don't support from your custom scanner.  Note that defining
   ;; new pattern->scanner methods is not supported.
   #:pattern->scanner
   ;; If you want to specify an alternative extraction behavior for matched
   ;; vars.
   #:*extractor*
   ;; To change the default "space only" behavior of LTRIM, RTRIM, and TRIM transforms.
   #:*trim-character-bag*
   ;; If you want to specify alternative transforms for matched vars.
   #:*options*)

  (:documentation "Provides a PARSE macro emulating the REXX programming language
namesakew with lexical bindings and extensible 'template' (REXX nomenclature)
capabilities."))

