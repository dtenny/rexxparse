(in-package :cl-user)

(defpackage :rexxparse-test
  (:use :cl :rexxparse :fiveam)
  (:export #:run-tests)
  (:documentation "Tests for the :rexxparse package."))

(in-package :rexxparse-test)

(def-suite test-suite :description ":rexxparse tests")
(in-suite test-suite)

;; Spaces in these text values are significant.
(defvar *text* 
  "2024/02/23 17:35:42.022 -  unable to locate '/usr/local/examples/' directory")
(defvar *text2* "This is  the text which, I think,  is scanned.")

(test string-patterns-and-basics
  "Test PARSE with string patterns and basic REXX PARSE edge cases."

  ;; An empty string is never found, it always matches the end of the source string.
  (is (equalp '(" abc ") (parse " abc " (a ""))))
  (is (equalp '(" abc " "") (parse " abc " (a "" b))))

  (is (null (parse "abc" ())))
  (is (equalp '("abc" "") (parse "abc" (a b))))
  (let ((rexxparse:*UNMATCHED-BINDING-VALUE* nil))
    (is (equalp '("abc" "" nil) (parse "abc" (a b c)))))
  (is (equalp '("a" "b c") (parse "a b c" (a b))))
  (is (equalp '("a" "b") (parse "a b c" (a b " "))))
  (is (equalp '(" a b c ") (parse " a b c " (a))))
  (is (equalp '("a" "b c") (parse " a b c" (a b))))
  (is (equalp '("a" "b c") (parse " a b c" (" " a b))))
  (is (equalp '() (parse "a b c" (_))))
  (is (equalp '("a" "c") (parse "a b c" (a _ c))))
  (is (equalp '("a" "b c " " g") (parse "a b c x g" (a b "x" g))))

  ;; with one var, always matches whole string
  (is (equalp '("   ") (parse "   " (stuff))))
  ;; with two vars, both bound to empty strings due to word splitting?
  (is (equalp '("" "") (parse "   " (a b))))
  (is (equalp '("" "" "") (parse "   " (a " " b c))))
  ;; This is pretty central to understanding splits, because the 
  ;; un-trimmed last binding principle applies _to each split_
  ;; Thus there's a split at ".", and "Q" is not trimmed.  However there's 
  ;; still the word-splitting blank elimination after "John".
  (is (equalp '("John" "     Q" "   Public")
              (parse "    John      Q.   Public" (fn init "." ln))))
  ;; I believe that for the next example, the blank is omitted from the 'is'
  ;; variable because of the leading space elimination because one of the
  ;; pattern goalposts is the implicit word-splitting pattern.
  (is (equalp '("Now" "is" "the time")
              (parse "Now  is the time" (now " " is the-time))))

  ;; OOREXX manual 42.2.2 Parsing strings into words
  ;; The word splitting is not the same as searching for " " in the pattern
  ;; which is why this test has this expected value
  ;; In this case the leading space for ' I think' on w2, is preserved
  ;; because the implicit non-word-splitting binding context applies.
  ;; Still guessing though.
  (is (equalp '("This is  the text which" " I think" "  is scanned.")
              (parse *text2* (w1 "," w2 "," w3))))
  (is (equalp '("This is  the text which" " I think" "  is scanned." "")
              (parse *text2* (w1 "," w2 "," w3 "," w4))))
  (is (equalp '("This" "is" "the" "text which") (parse *text2* (w1 w2 w3 w4 ","))))
  (is (equalp '("This" "is" "" "the text which")
              (parse *text2* (w1 " " w2 " " w3 " " w4 ","))))

  ;; Unmatched content, we specify a colon before the millis when a period
  ;; was in the input.
  ;; When a match for a pattern cannot be found, it matches the end of the string.
  (is (equalp '("2024" "02" "23" "17" "35" 
                "42.022 -  unable to locate '/usr/local/examples/' directory"
                "" "")
              (parse *text* (year "/" month "/" day hours ":" minutes ":" seconds
                                  ":" millis "-" rest))))
  ;; Note the blank following 022.
  (is (equalp '("2024" "02" "23" "17" "35" "42" "022 "
                "  unable to locate '/usr/local/examples/' directory")
              (parse *text* (year "/" month "/" day hours ":" minutes ":" seconds
                                  "." millis "-" rest))))

  ;; Sexp source that is not a string but produces a string
  (flet ((foo () "bar"))
    (is (equalp '("b" "r") (parse (foo) (b "a" r)))))

  ;; Source that is not a string.
  (signals type-error (parse #\c (a)))

  ;; Inappropriate use of keywords as variables, which are symbolp.
  ;; Trying to test this condition may be more trouble than it's worth...
  (signals error (macroexpand '(parse "a b" (:a))))

  ;; Non-default body and return value
  (is (eq 'xyz
          (let (a b c)
            (parse "a b c" (x y z)
              ;; just a couple of statements to mess around
              (setq a x b y c z)
              (is (equalp '("a" "b" "c") (list a b c)))
              'xyz))))
  )

(test standard-options
  ;; Equal, at least on SBCL, respects case, while EQUALP does not.
  ;; And we want these tests to respect case
  (is (not (equal "a" "A")))
  (is (not (equal '("a") '("A"))))

  ;; Test conditions raised during macro expansion
  (signals error (macroexpand '(parse :upper :lower "A" (w))))

  (is (equal '("a b " " d")
             (parse :lower "A b C d" (w "c" r))))
  (is (equal '("a b c d" "")
             (parse :lower "A b C d" (w "C" r))))
  (is (equal '("A B " " D")
             (parse :upper "A b C d" (w "C" r))))
  (is (equal '("A B C D" "")
             (parse :upper "A b C d" (w "c" r))))
  (is (equal '("A b " " d")
             (parse :caseless "A b C d" (w "c" r))))
  )

(test positions
  ;; Absolute positions

  ;;               1         2         3
  ;;      1234567890123456789012345678901234
  (is (equalp '("Brimfield    " "Massachusetts   " "10101")
              (parse "Brimfield    Massachusetts   10101" (city 14 state 30 zip))))

  (is (equalp '(" ab" " a") (parse " abc " (ab "c" 1 c 3))))

  ;; The position 1 is similar to an empty string, it is never "found"
  ;; (in the sense of a pattern being found).
  (is (equalp '("abc" "abc") (parse "abc" (a 1 bc))))
  (is (equalp '(" abc " " abc ") (parse " abc " (a 1 bc))))
  (is (equalp '("a" "bc") (parse "abc" (a 2 bc))))
  (let ((x 1))
    (is (equalp '("abc" "abc") (parse "abc" (a (= x) bc))))
    (is (equalp '(" abc " " abc ") (parse " abc " (a (= x) bc))))
    (is (equalp '("a" "bc") (parse "abc" (a (= (+ x 1)) bc)))))

  (is (equalp '("st" "a" "r" "s")
              (parse "astronomers" (2 st 4 1 a 2 4 r 5 11 s))))

  ;; Invalid/edge-case absolute positions
  ;; Basically too far to the left treated like "1".
  ;; Too far to the right treated as the whole (or remaining) string matches.
  (is (equalp '(" abc ") (parse " abc " (-0 x -0))))
  (is (equalp '(" abc ") (parse " abc " (-1 x -1))))
  (is (equalp '(" abc ") (parse " abc " (0 x 0))))
  (is (equalp '(" abc ") (parse " abc " (1 x 1))))
  (is (equalp '(" abc") (parse " abc " (x 5))))
  (is (equalp '(" abc" " ") (parse " abc " (x 5 x2))))
  (is (equalp '(" abc ") (parse " abc " (x 6))))
  (is (equalp '(" abc ") (parse " abc " (x 7))))
  (is (equalp '(" abc " "") (parse " abc " (x 7 x2))))

  ;; Relative positions
  (is (equalp '(" a" "abc ") (parse " abc " (x "b" (- 1) y))))
  (is (equalp '(" a" " abc ") (parse " abc " (w1 "b" (- 5) w2)))); no such "match" for offset -5

  ;; template with "string" var <relative-positional> is a special case,
  ;; var _includes_ string pattern which would normally be skipped
  ;; (after some very similar non-relative positional matches for context/validation)
  (is (equalp '(" c ") (parse " a b c " ("b" b))))     ; "b" not included, no relative positional
  ;; '5' is effectively equal to the source scanning start position when its pseudo-pattern
  ;; is matched, which means an empty string match, which is a break/tail-position behavior.
  (is (equalp '(" c ") (parse " a b c " ("b" b 5))))   ; from end of "b" to 5 is empty, full break
  (is (equalp '(" c")  (parse " a b c " ("b" b 7))))   ; space past "b" to 7, 2 chars
  (is (equalp '("b") (parse " a b c " ("b" b (+ 1))))) ; from position of b to position+1
  (is (equalp '("b c ") (parse " a b c " ("b" b (- 1))))) ; from position of b to end of string
  (is (equalp '(" a" "bc " "abc ") (parse " abc " (x "b" y (- 1) z))))

  ;; This is the relative version of the absolute 'stars' position match above
  (is (equalp '("st" "a" "r" "s")
              (parse "astronomers" (2 st (+ 2) (- 3) a (+ 1) (+ 2) r (+ 1) (+ 6) s))))
  
  (is (equalp '("RE" "X" "X") (parse "REstructured eXtended eXecutor"
                                     (v1 3 _ "X" v2 (+ 1) _ "X" v3 (+ 1) _))))

  ;; various bounds cases
  (is (equalp '("a" "b" "c") (parse "abc" (0 v1 2 v2 3 v3 4))))
  (is (equalp '("a" "b" "c") (parse "abc" ((- 2) v1 (+ 1) v2 (+ 1) v3 (+ 3)))))
  (is (equalp '("abc") (parse "abc" ((- 2) v1))))
  (is (equalp '("") (parse "abc" ((+ 12) v1))))
  (is (equalp '("") (parse "abc" (12 v1))))
  (is (equalp '("abc") (parse "abc" (v1 (- 2)))))
  (is (equalp '("abc") (parse "abc" (v1 (+ 12)))))
  (is (equalp '("abc") (parse "abc" (v1 12))))
  (is (equalp '("ab") (parse "abc" ("a" (+ 0) b 3))))
  (is (equalp '("ab") (parse "abc" ("a" (- 0) b 3))))
  )

(test length-positions
  ;; Testing several things here.
  ;; 1. Length positions vs relative positions, which are mostly the same but not always, 
  ;; particularly for zero.
  ;; 2. References to previous bindings later in the template
  ;; 3. Implicit conversion of strings to integers for positional patterns.

  ;; Parsing with relative patterns only.  'middle' being the tricky bit.
  (is (equalp '("Mark" "05Twain" "Twain" "05")
              (parse "04Mark0005Twain" 
                     (len (+ 2) first (+ len) len (+ 2) middle (+ len) len (+ 2) last (+ len))
                     (list first middle last len))))

  ;; Parsing with positional length patterns.
  (is (equalp '("Mark" "" "Twain" "05")
              (parse "04Mark0005Twain" 
                     (len (+ 2) first (> len) len (+ 2) middle (> len) len (+ 2) last (> len))
                     (list first middle last len))))

  ;; Parsing with length patterns
  (is (equalp '("5" "5.6789") 
              (parse "12345.6789" ("." digit (< 1) rest))))
  ;; Parsing with relative patterns
  (is (equalp '("5" ".6789")
              (parse "12345.6789" ("." (- 1) digit (+ 1) rest))))

  ;; For no particular reason other than it messeed with my head at one point
  (is (equalp '("a b" "a b") (parse "a b" (1 w1 (+ 0) w2))))
  (is (equalp '("" "a b") (parse "a b" (1 w1 (> 0) w2))))

  ;; </> under/over flows
  (is (equalp '("" "12345.6789") 
              (parse "12345.6789" (1 digit (< 1) rest))))
  (is (equalp '("12345" "12345.6789") 
              (parse "12345.6789" ("." digit (< 5) rest))))
  (is (equalp '("12345" "12345.6789") 
              (parse "12345.6789" ("." digit (< 6) rest))))
  (is (equalp '(".67" "89") 
              (parse "12345.6789" ("." digit (> 3) rest))))
  (is (equalp '(".678" "9") 
              (parse "12345.6789" ("." digit (> 4) rest))))
  (is (equalp '(".6789" "") 
              (parse "12345.6789" ("." digit (> 5) rest))))
  (is (equalp '(".6789" "") 
              (parse "12345.6789" ("." digit (> 6) rest))))
  )

(test var-reuse
  (is (equalp '("b") (parse "abc" (w 2 w 3))))
  (is (equalp '("") (parse "abc" (w w))))
  (is (equalp '("") (parse "abc def" (w w w))))
  )

(test variable-string-patterns
  (is (equalp '("the quick " " fox")
              (let ((x "brown"))
                (parse "the quick brown fox" (start ($ x) end)))))
  )

;; As I intend to use this elsewhere (in some flavor for a clojure-like
;; `with-redefs`), I've made a few notes for future documentation.

(defmacro with-redef ((f-sym function) &body body)
  "Redefine the global function definition of symbol F-SYM
with the function FUNCTION with a lexical scope wrapping BODY.

FUNCTION must be an object of type FUNCTION, not some other function designator, 
compatible with (setf (fdefinition f-sym) <function>).

Execute BODY with the rebound function, restoring the original function (or lack thereof)
on exit.  F-SYM need not be FBOUNDP to start with.

Note that this macro may have unsafe effects in a multi-threaded use of the
symbol unless the caller arranges additional critical-section logic.
Also note compiler transformations or inlining may also result in surprises
when it comes to redefining a function, as well as use of compiled symbol-function
references that have previously nabbed the function and which won't see changes made 
after the fact.

Warning: If the new function attempts to call the old function, make sure it isn't via the
the function being redefined.  E.g.

  ;; This will be an infinite loop or stack overflow
  (with-redef (my-fun (lambda () ... stuff ... (funcall 'my-fun))))

  ;; ;This will work
  (let ((old-fun #'my-fun))
    (with-redef (my-fun (lambda () ... stuff ... (funcall old-fun)))))

Returns the value(s) returned by BODY."
  ;; Don't really like this implementation, it'll do for the limited test here.
  ;; Would prefer to unwind-protect the setting of the symbol function as well as the 
  ;; restoration, among other things.  Also, we should probably use FDEFINITION
  ;; instead of SYMBOL-FUNCTION, so we can do SETF functions as well as plain symbols.
  ;; Or something like that.
  ;; Note effect on generic functions/methods?
  (let ((old (gensym))
        (fun (gensym)))
    (declare (ignorable old))
    `(let ((,fun ,function))
       (assert (typep ,fun 'cl:function))
       (cond
         ((fboundp ',f-sym)
          (let ((,old (symbol-function ',f-sym)))
            (setf (symbol-function ',f-sym) ,fun)
            (unwind-protect (progn ,@body)
              (setf (symbol-function ',f-sym) ,old))))
         (t
          (setf (symbol-function ',f-sym) ,fun)
          (unwind-protect (progn ,@body)
            (fmakunbound ',f-sym)))))))

;; My with-redef as of May-15-2024 doesn't seem to work on ECL and perhaps other
;; tests and it isn't worth making it work for all platforms because the
;; macroexpansion logic is the same across all platforms. So if it (correct
;; parse behavior with respect to this test) works for one platform, we know it
;; works for all
#+SBCL
(test no-useless-extractions
  (let ((counter 0)
        (old-extractor #'rexxparse::extract))
    (flet ((extractor (&rest args) 
             (incf counter)
             (apply old-extractor args)))
      (with-redef (rexxparse::extract #'extractor)
        (is (null (parse "abc" (_))))
        (is (zerop counter))
        (is (equalp '("abc") (parse "abc" (a))))
        (is (= counter 1))
        (is (equalp '("b") (parse "abc" (2 3 (- 1) c (+ 1)))))
        (is (= counter 2))
        ))))

(test transforms
  ;; Upper lower, first & last positions, tail positions
  (is (equalp '("A" "b" "C") (parse "a b c" ((upper a) b (upper c)))))
  (is (equalp '("a" "B" "c") (parse "a b c" (a (upper b) c))))
  (is (equalp '("C D") (parse "a b c d" (_ _ (upper x)))))
  (is (equalp '("a" "B" "c") (parse "A B C" ((lower a) b (lower c)))))
  (is (equalp '("A" "b" "C") (parse "A B C" (a (lower b) c))))
  (is (equalp '("c d") (parse "A B C D" (_ _ (lower x)))))

  ;; SNAKE, KEBAB, LTRIM, RTRIM, TRIM, *TRIM-CHARACTER-BAG*
  (is (equalp '("kebab_case" "to_snake_case")
              (parse "kebab-case to-snake-case" ((snake a) (snake b)))))
  (is (equalp '("snake-case" "to-kebab-case")
              (parse "snake_case to_kebab_case" ((kebab a) (kebab b))))) 
  (is (equalp '("  ab" "  def  " "hi  ") (parse "  abc  def  ghi  " (a "c" d "g" g))))
  (is (equalp '("ab" "def" "hi") (parse "  abc  def  ghi  " ((ltrim a) "c" (trim d) "g" (rtrim g)))))

  (let ((s (coerce (list #\space #\newline #\a #\b #\c #\space #\newline) 'string))
        (s-with-newline (coerce (list #\newline #\a #\b #\c #\space #\newline) 'string))
        (s-without-newline "abc"))
    (is (equalp (list s-with-newline) (parse s ((trim x)))))
    (let ((*trim-character-bag* (list #\space #\newline)))
      (is (equalp (list s-without-newline) (parse s ((trim x)))))))

  ;; INTEGER, FLOAT, DOUBLE, KEYWORD
  (parse "1" ((integer one))
         (is (integerp one))
         (is (= 1 one)))
  (parse "1.0 2.0" ((float one) (double two))
         (is (typep one 'single-float))
         (is (typep two 'double-float))
         (is (= 1 (round one)))
         (is (= 2 (round two))))
  (signals error (parse "fred" ((integer x))))
  (signals error (parse "fred" ((float x))))
  (signals error (parse "fred" ((double x))))

  ;; Assumes upper case symbols on the lisp.
  (parse "abc" ((keyword x))
         (is (keywordp x))
         (is (not (eq :abc x)))
         (is (eq :|abc| x)))
  (parse :upper "abc" ((keyword x))
         (is (keywordp x))
         (is (eq :abc x)))

  ;; The TRANSFORM transform and various function designators
  (let (y)
    (flet ((saver (val) (setq y val)))
      (is (equalp '("a b c") (parse "a b c" ((transform a #'saver)))))
      (is (equalp "a b c" y))))

  ;; #'(lambda ...)
  (let (y)
    (is (equalp '("A B C") 
                (parse "a b c" ((transform a #'(lambda (x) (setq y (string-upcase x))))))))
    (is (equalp "A B C" y)))

  ;; (lambda ...)
  (let (y)
    (is (equalp '("A B C") 
                (parse "a b c" ((transform a (lambda (x) (setq y (string-upcase x))))))))
    (is (equalp "A B C" y)))

  (is (equalp '("A" "b") (parse "a b" ((transform a 'string-upcase) b))))
  (is (equalp '("A" "b") (parse "a b" ((transform a #'string-upcase) b))))


  ;; Bogus transforms
  (signals error (macroexpand '(parse "a b c" ((foo a)))))
  (signals error (macroexpand '(parse "a b c" ((foo)))))
  )

(test declarations
  (is (equalp '("a" 2)
              (parse "a 1" (a (integer one))
                     (declare (string a) (integer one))
                     (list a (1+ one))))))

(test using
  (let ((a 0))
    (is (equalp '("a" "b") (parse :using (a) "a b" (a b))))
    (is (equalp "a" a))
    (is (equalp '("a" "b") (parse (:using a) "a b" (b a)
                                  (list b a))))
    (is (equalp "b" a)))

  (let ((v (make-array 4 :fill-pointer 0 :adjustable nil)))
    (is (equalp '("a" "b") (parse :using-vector (v) "a b" (v b))))
    (is (equalp #("a") v))
    ;; Explicit body reference to V is going to return the vector, not what was matched
    ;; and stuffed into the vector.  This is by design. User supplied body has full control
    ;; (and responsibility for any mess that arises).
    (is (equalp (list "a" v) (parse (:using-vector v) "a b" (a v) 
                                    (list a v))))
    (is (equalp #("a" "b") v))          ;accumulated across two parse invocations
    (is (null (parse (:using-vector v) "c d" (v v) nil)))
    (is (equalp #("a" "b" "c" "d") v))
    ;; Vector-push silently does nothing if vector is full.
    (is (equalp '("e") (parse (:using-vector v) "e" (v))))
    (is (equalp #("a" "b" "c" "d") v)))

  ;; Vector and positional patterns
  (let ((v (make-array 4 :fill-pointer 0)))
    (is (equalp #("04" "Mark")
                (parse (:using-vector v) "04Mark" (1 v (+ 2) v (> (aref v 0))) v))))

  ;; Mix it up!
  (let (a b (v1 (make-array 4 :fill-pointer 0)) (v2 (make-array 4 :fill-pointer 0)))
    (is (equalp '("a" "b" "c" "d" "e")
                (parse (:using a b) (:using-vector v1 v2) "a b c d e" (a b v1 v2 e))))
    (is (equalp "a" a))
    (is (equalp "b" b))
    (is (equalp #("c") v1))
    (is (equalp #("d") v2))
    (is (not (boundp 'e))))

  ;; Negative tests.

  ;; Symbols may only be in :USING or :USING-VECTOR, not both
  (signals error (macroexpand '(parse :using (a b e) :using-vector (b c d a) "abc" (a b c d e))))

  ;; _ may not be in :USING or :USING-VECTOR
  (signals error (macroexpand '(parse :using (a _ c) "abc" (a c))))
  (signals error (macroexpand '(parse :using-vector (a _ c) "abc" (a c))))

  ;; :USING-[VECTOR] - lists may be empty, but must contain (non-keyword) symbols if non-empty
  (is (equalp '("a" "b") (parse :using () "a b" (a b))))
  (is (equalp '("a" "b") (parse :using-vector () "a b" (a b))))
  (signals error (macroexpand '(parse :using (:fred) "a b" (a b))))
  (signals error (macroexpand '(parse :using-vector (1) (a b) "a b" (a b))))
  )

(defun run-tests ()
  "Run all :rexxparse tests"
  (run 'test-suite)
  nil)

;;(trace rexxparse::match-and-extract rexxparse::scan-string rexxparse::scan-word-split rexxparse::extract rexxparse::extract-after-left-trim rexxparse::scan-absolute-position rexxparse::pattern->scanner rexxparse::scan-leftward-relative-position rexxparse::scan-rightward-relative-position REXXPARSE::SCAN-LEFTWARD-LENGTH-POSITION REXXPARSE::SCAN-RIGHTWARD-LENGTH-POSITION)

;; Just a tool so I don't have to edit a 'foo.rexx' file and run rexx on it to compare
;; to what rexx does. Don't use it in tests, it would add a very undesirable dependency (OORexx)
(defun trexx (source template &key debug)
  "Try parsing SOURCE with TEMPLATE, both strings, in REXX, and returning a list of 
bound variables the way PARSE would.  Period placeholders will not be in the results.
Template should be a string containing rexx-syntax template data.

E.g. (trexx \"abc\" \"v1 2 v2 3 . 4\") => (\"a\" \"b\")

Assumes REXXPARSE:PARSE is working enough to parse the REXX output  :-)
Assumes newlines for line separators, sorry Winblows and Fruit OSes.

Only works works if 'rexx' is in your PATH.  Author tests with Open Object Rexx."
  (let ((rexx-output
          (uiop/stream:call-with-temporary-file 
           (lambda (pathname) 
             (with-open-file (s pathname :direction :output :if-exists :supersede)
               (format s "trace i~%parse value '~a' with ~a~%" source template))
             (with-output-to-string (stream)
               #+NIL(uiop:run-program (list "cat" (namestring pathname)) :output stream)
               (uiop:run-program (list "rexx" (namestring pathname)) 
                                 :error-output stream
                                 :output stream)))
           :want-pathname-p t :want-stream-p nil)))
    ;; For (trexx "abc" "v1 2 v2")
    ;; Looking for output lines like        >=>   V2 <= \"bc\"
    ;; Report last binding if var bound multiple times.
    (let ((rexxparse:*unmatched-binding-value* nil))
      (with-input-from-string (stream rexx-output)
        (loop with result = nil
              for line = (if debug (print (read-line stream nil nil)) (read-line stream nil nil))
              while line
              do (parse line (">=>   " name " <= " val)
                   (when val
                     (parse val ("\"" v "\"")
                        (setf result (acons name v result)))))
              finally (return (nreverse
                               (mapcar #'cdr 
                                       (delete-duplicates result :key #'car :test #'equalp 
                                                                 :from-end t)))))))))
