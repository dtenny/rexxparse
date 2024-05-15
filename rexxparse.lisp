(in-package :rexxparse)

;;;; Types of functions in this module.
;;;;
;;;; 1. Scanning functions. These scan text for some pattern known to the function
;;;;    beginning at some starting position.
;;;;
;;;;    Input arguments: 
;;;;    1. PARSE "source" text to be scanned, always a string.
;;;;    2. Fixnum indicating of the first position in source to start scanning.
;;;;    3. Fixnum indicating the position of the last pattern match. This is zero
;;;;       for the first scan.  This data is mostly needed for relative positional scans
;;;;       which are interpreted relative to start of the last pattern match.
;;;;    4. The "pattern" which is to be used for the scanning, e.g. the string
;;;;       to be sought in the source text, a POSITION directive to the scanner,
;;;;       or a regexp or other custom representation used by a custom scanner
;;;;       of your deriving based on a scanner lookup function you provide via
;;;;       *PATTERN->SCANNER*.
;;;;
;;;;    Not all scanners need all arguments, the arguments are supplied for extensible
;;;;    scanner API purposes.
;;;;
;;;;    Scanners return NIL if the pattern is not found, otherwise 
;;;;    they two two values:
;;;;    - the position within the input text of the first character matched
;;;;    - the position following the last character of the matched pattern.
;;;;      Note that for positional patterns the two values will generally be EQL.
;;;;  
;;;; 2. Extract functions. Extract functions are called when the scanner
;;;;    identified a (possibly zero length) region of text to extract from 
;;;;    the "source".
;;;;
;;;;    Input arguments:
;;;;    1. The PARSE "source" text of which some subsequence is to be extracted.
;;;;    2. The inclusive starting position of the region to extract.
;;;;    3. The exclusive end position of the region to extract.
;;;;
;;;;    Extractors always return strings. The strings returned by the extractor
;;;;    should be considered immutable.
;;;;
;;;; 3. Transform functions. These take a string as input and produce any type of
;;;;    of value as output. 
;;;;
;;;; WARNING:
;;;; All strings bound or returned by PARSE should be considered immutable.
;;;; We prefer to avoid consing unnecessary string copies in this implementation.

;;; Reminder to self, `string=` works on _string designators_ include symbols
;;; and looks at the name of symbols so no need to deref with `symbol-name` first.

;;; Original REXX PARSE algorihm description which was surprisingly unreadable to me, but FWIW:
;;; https://www.ibm.com/docs/en/zos/2.1.0?topic=parsing-details-steps-in

(alexandria:define-constant +empty-string+ "" :test #'string=)

(defvar *UNMATCHED-BINDING-VALUE* +empty-string+
  "PARSE initializes bindings to this value. Unmatched bindings
will return this value. Rebind (outside the call to PARSE)
if you want a different outcome.")

(defun extract (term start end)
  "Extraction only. No transformation, not even blank spaces.
Extract text in string TERM from START inclusive to END exclusive after
Return a new string with the extracted content."
  (declare (string term) (fixnum start end))
  (subseq term start end))

(defun extract-after-left-trim (term start end)
  "Transformer & Extraction.
Extract text in string TERM from START inclusive to END exclusive after
trimming leading space characters (and ONLY leading space characters).
Return a string with the trimmed content."
  ;; Note that REXX guide says:
  ;; "Leading blanks are removed from each word in the string before it is assigned
  ;; to a variable, as is the blank that delimits the end of the word."
  (declare (string term) (fixnum start end))
  (loop while (and (< start end)
                   (char= #\space (schar term start)))
        do (incf start))
  (subseq term start end))

(defun scan-absolute-position (source source-start last-match-start position)
  "Pseudo-scanner for an absolute one-based position. 

Valid (one-based) positions/columns are from one to length of the source.
Values less than 1 are treated like 1. Values > length are treated as length.

If the position is not a valid column in the source string, return NIL
for the match start pos, and (1- POSITION) for the match-follow-pos.
This is similar to searches for empty string patterns, except that
we are going to set the start position.

If the position denotes a valid column in the source string,
return two values, (1- POSITION) for the match start position
and (1- POSITION) (as if we scanned a zero length string
pattern without the 'no empty patterns' rule).

Note: position 1 is like an empty string pattern, it is never found.
However unlike an empty string it may modify the source start position."
  (declare (string source) (ignore last-match-start) (fixnum source-start position))
  (let ((position (1- position)))       ;to zero-based index
    (when (< position 0) (setq position 0))
    (if (= position source-start)       ;special "not found" semantics
        (values nil position)
        (let ((len (length source)))
          (when (> position len) (setq position len))
          (if (< position len)
              (values position position)
              nil)))))

(defun scan-leftward-relative-position (source source-start last-match-start offset)
  "Pseudo-scanner for a leftward relative offset from the last match.

OFFSET must be be a non-negative fixnum, which is subtracted from LAST-MATCH-START
to obtain a new index into SOURCE.

If the new position is not a valid column in the source string, return NIL
for the match start pos, and (1- SOURCE-LENGTH) for the match-follow-pos.
This is similar to searches for empty string patterns, except that
we are going to set the start position.

If the new position denotes a valid column in the source string,
return two values, (- LAST-MATCH-START OFFSET) for the (new) match start position
and (- LAST-MATCH-START OFFSET) (as if we scanned a zero length string
pattern without the 'no empty patterns' rule).

Note1: position 1 is like an empty string pattern, it is never found.
However unlike an empty string it may modify the source start position.

Note2: This pseudo-scanner for positions can return a scan position that precedes SOURCE-START,
which is not a valid subsequence.  The caller should deal with with it accordingly."
  (declare (ignore source source-start) (fixnum last-match-start offset))
  ;;(check-type offset (integer 0 #.most-positive-fixnum)) now enforced by caller
  (let ((position (- last-match-start offset)))
    (when (< position 0) (setq position 0))
    (if (= position 0)                  ;special "not found" semantics
        (values nil position)
        (values position position))))

(defun scan-leftward-length-position (source source-start last-match-start offset)
  "Pseudo-scanner for a leftward length offset from the last match with `<` semantics.
Arguments are as for `scan-leftward-relative-position`."
  (declare (ignore source source-start) (fixnum last-match-start offset))
  ;;(check-type offset (integer 0 #.most-positive-fixnum)) now enforced by caller
  (let ((position (- last-match-start offset)))
    (when (< position 0) (setq position 0))
    (values position last-match-start)))

(defun scan-rightward-relative-position (source source-start last-match-start offset)
  "Pseudo-scanner for a rightward relative offset from the last match.

OFFSET must be be a non-negative fixnum, which is added to LAST-MATCH-START
to obtain a new index into SOURCE.

If the resulting new position is not a valid column in the source string
(exceeding the length of the string) , return NIL
for the match start pos, and SOURCE-LENGTH for the match-follow-pos.
This is similar to searches for empty string patterns.

If the resulting position denotes a valid column in the source string,
return two values, (+ LAST-MATCH-START OFFSET) for the (new) match start position
and (+ LAST-MATCH-START OFFSET) (as if we scanned a zero length string
pattern without the 'no empty patterns' rule)."
  (declare (ignore source-start) (string source) (fixnum last-match-start offset))
  ;;(check-type offset (integer 0 #.most-positive-fixnum)) now enforced by caller
  (let ((max-length (length source))
        (position (+ last-match-start offset))) ;last-match-start already zero-based
    (if (>= position max-length)
        (values nil max-length)
        (values position position))))

(defun scan-rightward-length-position (source source-start last-match-start offset)
  "Pseudo-scanner for a rightward length (>) position, similar to, but different from, '+'.
Where + on overflow would not match, > does match."
  (declare (ignore source-start) (string source) (fixnum last-match-start offset))
  ;;(check-type offset (integer 0 #.most-positive-fixnum)) now enforced by caller
  (let ((max-length (length source))
        (position (+ last-match-start offset))) ;last-match-start already zero-based
    (when (>= position max-length)
      (setq position max-length))
    (values position position)))

(defvar *string-equality-predicate* #'string=
  "Predicate used to compare strings in `scan-string`.")

(defun scan-string (source source-start last-match-start string)
  "Scanner. Find STRING in SOURCE beginning at SOURCE-START.
If not found, return NIL.
Otherwise return two values, the starting position in STRING in SOURCE,
and the position of the character following the occurrence of STRING in SOURCE.

Note: an empty string is never found."
  (declare (string source string) (fixnum source-start) (ignore last-match-start))
  (if (zerop (length string))
      nil
      (let ((match-pos (search string source 
                               :start2 source-start
                               :test *string-equality-predicate*)))
        (if match-pos
            (values match-pos (+ match-pos (length string)))
            nil))))

(defun scan-word-split (source start last-match-start unused) ; unused == :word-split-pattern
  "Scanner. Search for word-splitting spaces (or end of string) beyond a 
word/token which may have preceding spaces we need to skip.

If we don't find a word token, return NIL.
Otherwise return two values:
1. the position in SOURCE of the first space following the word token,
   or the length of SOURCE if there are no spaces following.
2. value 1 + 1 if a space was found, or value 1 if end-of-string applied.
"
  (declare (string source) (fixnum start) (ignore last-match-start unused))
  (let ((end (length source)))
    (declare (fixnum end))
    (loop while (and (< start end)
                     (char= #\space (schar source start)))
          do (incf start))
    (if (< start end)
        ;; Positioned at token text, will have non-nil return
        (loop with pos fixnum = (1+ start)
              while (and (< pos end)
                         (char/= #\space (schar source pos)))
              do (incf pos)
              finally (return (values pos (min (1+ pos) end))))
        ;; There was no token, return nil
        nil)))

(defparameter *builtin-list-pattern-scanners*
  (list (cons := 'scan-absolute-position)
        (cons :- 'scan-leftward-relative-position)
        (cons :+ 'scan-rightward-relative-position)
        (cons :< 'scan-leftward-length-position)
        (cons :> 'scan-rightward-length-position)
        (cons :$ 'scan-string))
  "An association list keyed by pattern operator keyword (the canonicalized form
of user symbols specified for list patterns like `(+ <s-exp>)` which were originally 
symbols from potentially any package).

Values are function symbols which should perform the scan when invoked as
`(fn source-string start-position <s-exp>)` (Similar to `scan-string`).")

(defun aget (key alist &key (test #'eql) not-found)
  "Similar to ASSOC but returns the value of the cons whose car was KEY instead of the
cons itself. Returns NOT-FOUND if there KEY isn't ALIST."
  (alexandria:if-let ((cons (assoc key alist :test test)))
    (cdr cons)
    not-found))

;;; Pattern representation after parsing template
;;; As these are created at macro processing time, and referenced in the expansion
;;; we need to arrange for the pattern struct be written/read in fasls with MAKE-LOAD-FORM

(defstruct (pattern (:constructor %make-pattern))
  sexp        ;original macro argument
  string      ;if sexp is a string. Presently nil for `($ x)` sexp though
  ;; Position isn't necessarily known until pattern is used while matching
  ;; it may rely on variables previously bound in the PARSE matching.
  ;; It is known if a number was specified as an absolute position pattern.
  position    ;integer value of pattern if fixnum sexp or positional-p
  operator    ;iff pattern is list or keyword, operator as a keyword
  relpos-p    ;true if operator is + or - relative position directive
  positional-p;true if operator is +/-/=/</>
  parameter   ;unevaluated argument symbol iff pattern is a list referencing a symbol
  )

(defun pattern->scanner (pattern)
  "Function that accepts a pattern structure and returns a function to scan
for the pattern in a PARSE source string. Note that some patterns are directives more than
things saught, for example so-called positional patterns. 

See package README.md file for more information on valid pattern literals.

If you want to specify a different scanner for a pattern in some extension,
bind the *PATTERN->SCANNER* special variable, which shouild call
this function if it doesn't wish to handle a particular pattern descriptor."
  (let ((sexp (pattern-sexp pattern)))
    (typecase sexp
      (list (or (aget (pattern-operator pattern) *builtin-list-pattern-scanners*)
                (error "Unknown pattern action ~s in ~s.~%Valid actions are ~{~a~^, ~}"
                       (pattern-operator pattern) sexp
                       (mapcar #'car *builtin-list-pattern-scanners*))))
      (fixnum 'scan-absolute-position)
      (string 'scan-string)
      (t (error "Unable to derive a scanner for pattern literal ~s" sexp)))))

(defvar *pattern->scanner* 'pattern->scanner
  "A function designator which takes as input a valid PARSE pattern literal
such as a string, position designator, or `MATCH` expression, and should return
a function to scan for the literal in a PARSE source string.

This variable defaults to *FINISH*. It is expressed as a dynamic variable
so that you can rebind it if you have special pattern needs that aren't supported
by the default REXXPARSE behavior.

The resulting scanner function should accept arguments:
- source string to be scanned
- start position within the source string for scanning
- pattern literal that was input to the call to the *PATTERN->SCANNER* function.

The scanner function should return:
- the match position of the start of the pattern in the input source string.
- the position of the first character in the source string after the matched
- text.")

(defmethod make-load-form ((p pattern) &optional env)
  (declare (ignore env))
  `(%make-pattern :sexp ,(pattern-sexp p)
                  :string ,(pattern-string p)
                  :position ,(pattern-position p)
                  :operator ,(pattern-operator p)
                  :relpos-p ,(pattern-relpos-p p)
                  :positional-p ,(pattern-positional-p p)
                  :parameter ,(pattern-parameter p)))

(defun make-pattern (s-exp)
  "Allocate and initialize a pattern struct given some macro s-expression representing a pattern.
s-exp may one of a set of keywords denoting special pattern activity, such as
:word-split-pattern or :end-of-text-pattern."
  (let* ((operator (if (listp s-exp) (car s-exp) nil))
         (operator-keyword (cond ((keywordp s-exp) s-exp)
                                 ((keywordp operator) operator)
                                 ((symbolp operator) (intern (symbol-name operator) :keyword)))))
    (%make-pattern :sexp s-exp
                   :string (if (stringp s-exp) s-exp nil)
                   :position (if (integerp s-exp) s-exp nil)
                   :operator operator-keyword
                   :relpos-p (or (eq :- operator-keyword) (eq :+ operator-keyword))
                   :positional-p (and (member operator-keyword '(:+ :- := :> :<) :test #'eq) t)
                   :parameter (if (listp s-exp) (second s-exp) nil))))
                 
(defun pattern-argument (pattern)
  "Return the self-evaluating value of a pattern if present, or the pattern parameter otherwise.
Note that pseudo-patterns such as :end-of-text-pattern will have no argument and will return NIL."
  (or (pattern-string pattern)
      (pattern-position pattern)
      (pattern-parameter pattern)))

(defun pattern? (s-exp)
  "Return true if s-exp indicates a pattern in a template (vs a binding symbol)
Valid patterns are:

    <pattern> ::= string-literal
    <pattern> ::= <position>
    <pattern> ::= ( $ <sexp> )

    <position> ::= position-integer-literal
    <position> ::= ( + <position-integer> )
    <position> ::= ( - <position-integer> )
    <position> ::= ( = <position-integer> )
    
    <position-integer> ::= position-integer-literal
    <position-integer> ::= <sexp>

We have not fully validated entities for which we return T."
  (or (stringp s-exp)
      (integerp s-exp)
      (and (listp s-exp) 
           (symbolp (car s-exp))
           (member (car s-exp) '("+" "-" "=" "<" ">" "$") :test #'string=))))

(defun implicit-positional-pattern-fixnum (pattern pattern-arg)
  "Given a pattern structure and evaluated pattern-arg as per MATCH-AND-EXTRACT,
allow implicit conversion of a string valued pattern-arg to a non-negative fixnum
for positional s-expression patterns.  This is so we can use previously matched
numbers from the source string as numeric positions for +/-/=/</> positional patterns.

Return pattern-arg as-is if pattern is not a positional s-exp pattern,
otherwise attempt to convert it to a non-negative fixnum if it is not already such a thing.

Signal a condition if pattern is a positional pattern and pattern-arg isn't, or isn't convertable
to, a non-negative fixnum."
  (if (pattern-positional-p pattern)
      (let ((position 
              (or (pattern-position pattern)
                  (setf (pattern-position pattern)
                        (etypecase pattern-arg
                          (string (parse-integer pattern-arg))
                          (integer pattern-arg))))))
        ;; restartable with new position, assigns new valuje to position if it passes test
        ;; It doesn't actually have to be a fixnum, just in the 0+ range of a fixnum
        (check-type position (integer 0 #.most-positive-fixnum))
        position)
      pattern-arg))

(defun match-and-extract (source scan-start last-match-start pattern pattern-arg extract-p
                          &aux (operator (pattern-operator pattern)))
  "MATCH-EXTRACT-TRANSFORM

SOURCE is the string to be parsed.

SCAN-START is the zero-based offset in SOURCE where we begin scanning.
For all scans but the first, it is the position following a prior pattern match or 
positional pseudo-match. For the first scan it will be zero.

LAST-MATCH-START is the zero-based offset in SOURCE where the last pattern match _began_.
This is important for relative positional offsets which are relative to the start of
the last match.  For the first scan it will be zero.

PATTERN is a pattern structure with all the original pattern data derived from the template.

PATTERN-ARG is the self-evaluating or evaluated argument derived from a symbolic parameter in the
pattern. If the pattern had a variable reference, this is the value of the variable from the 
macro-expanded code. It should be a string or fixnum, unless we've added support for extensions
which would supply regular expressions or other data types.

EXTRACT-P is true if a value should be extracted for binding, nil if there is no
binding to be performed.

Given an input string SOURCE to be scanned, a starting position START,
and PATTERN being scanned for, do the following:

1. Invoke the scanner designated by (funcall *PATTERN->SCANNER* pattern)
   on the source string.

2. If the scanner indicated a match, invoke the extractor on the region 
   whose end was identified by the scanner, return the extracted 
   text and the position to resume scanning.

   Whether the extractor should eat leading spaces depends on two things:
   i.  the pattern preceding the current region must be an implicit word-split-pattern,
   ii. the pattern following the current region must not be an explicit pattern,
       as explicit patterns imply a logical source string match
       such that the variable being bound is considered a tail match.

3. If the scanner does not indicate a match, return the tail match text
   and nil (no position to resume scanning).

4. If there is a transform, invoke it on the result of the extraction.

Restating return values:
If the scanner found a match return three values:
- The value returned by the extraction.
- The starting position of the scanner match. For a text pattern this would be the position
  of the first character of the text within the source string.  For a positional pattern
  this would be the position indicated by the positional directive.
- The position of the next place in source to resume scanning. For a text pattern
  this is the position of the first character in source following the last character matched
  in the text pattern.  For a positional pattern this will be identical to the positional
  directive. 

If the scanner did not find a match, return three values:
- All unprocessed text in the source as-is, i.e. text from scan-start to end of source string.
- 0
- NIL

Note that the first value may be NIL if the transformation function
returns NIL, so it is the third value that is the determination of whether
additional scans might be performed, with the caveat that positional patterns
remaining to be processed could back up the scan position!
"
  (declare (string source) (fixnum scan-start last-match-start) (pattern pattern))
  (if (eq operator :end-of-text-pattern)
      ;; Special tail binding semantics, *TBD*: allow extract/transform semantics?
      (values (and extract-p (extract source scan-start (length source))) nil)
      ;; Select a scanner based on the full pattern expression, e.g. `(+ x)`
      ;; Invoke a scanner on the pattern expression value, which is either the 
      ;; pattern itself it's a literal (string, number, etc), or the second
      ;; value in the pattern if it's a list, e.g. `x` in `(+ x)`.
      (let* ((scanner (if (eq operator :word-split-pattern)
                          'scan-word-split
                          (funcall *pattern->scanner* pattern)))
             (extractor (if (eq operator :word-split-pattern)
                            'extract-after-left-trim
                            'extract))
             ;; Strings to integers for positional patterns, if necessary
             (pattern-arg (implicit-positional-pattern-fixnum pattern pattern-arg)))
        (declare (type (or function symbol) extractor scanner))
        (multiple-value-bind (match-start-pos match-follow-pos)
            (funcall scanner source scan-start last-match-start pattern-arg)
          (if match-start-pos
              (let ((value 
                      (and extract-p
                           (cond 
                             ((eq :< operator)
                              (funcall extractor source match-start-pos match-follow-pos))

                             ((eq :> operator)
                              (funcall extractor source last-match-start match-follow-pos))

                             ;; +/- relative positions based on last match _start_
                             ((pattern-relpos-p pattern)
                              (if (>= last-match-start match-start-pos)
                                  ;; relpos backward string break or zero offset '>'
                                  (funcall extractor source last-match-start (length source))
                                  ;; relpos forward
                                  (funcall extractor source last-match-start match-start-pos)))
                             
                             ;; Absolute positions, if in effect, might precede scan-start
                             ;; Could be a normal string or word-split match situation as well.
                             ((> match-start-pos scan-start)
                              (funcall extractor source scan-start match-start-pos))
                         
                             ;; Presumed absolute position preceding start of last pattern match.
                             (t "")))))
                (values value match-start-pos 
                        (if (eq :< operator)
                            match-start-pos
                            match-follow-pos)))
              ;; Special tail binding semantics again
              (values (and extract-p (funcall extractor source scan-start (length source))) 
                      0 match-follow-pos))))))

(defparameter *transform-names*
  '("UPPER" "LOWER" "SNAKE" "KEBAB" "LTRIM" "RTRIM" "TRIM" "INTEGER" "DOUBLE" "FLOAT" "KEYWORD")  
  "Symbol names that are valid binding 'predefined' transformation names expecting one argument.
The TRANSFORM operator expects two and is not in this list.")

;; *TBD* whether _all_ extracted strings passed to transforms are safe to use with destructive transforms.
;; For now transforms make copies.  Besides, we tell users not to modify returned strings.

(defun to-snake (str)
  "Transform that converts hyphens to underscores."
  (declare (string str))
  (substitute #\_ #\- str))

(defun to-kebab (str)
  "Transform that converts underscores to hyphens."
  (declare (string str))
  (substitute #\- #\_ str))

(defvar *trim-character-bag* (list #\space)
  "Sequence of characters as per CL:STRING-TRIM that should be considered whitespace
for removal by the LTRIM, RTRIM, and TRIM predefined transforms. You can bind this if
you like, but it will affect all trimming transforms in the scope of the binding.

The default value is just the space character.")

(defun left-trim (str)
  "Transform that trims leading spaces off of str, where whitespace is as defined in 
*TRIM-CHARACTER-BAG*."
  (declare (string str))
  (string-left-trim *trim-character-bag* str))

(defun right-trim (str)
  "Transform that trims trailing spaces off of str, where whitespace is as defined in 
*TRIM-CHARACTER-BAG*."
  (declare (string str))
  (string-right-trim *trim-character-bag* str))

(defun trim (str)
  "Transform that trims leading and trailing spaces off of str, where whitespace is as defined in 
*TRIM-CHARACTER-BAG*."
  (declare (string str))
  (string-trim *trim-character-bag* str))

(defun to-float (str)
  "Transform that convers a string to single-float representation."
  (declare (string str))
  (parse-float:parse-float str :type 'single-float))

(defun to-double (str)
  "Transform that converts a string to double-float representation."
  (declare (string str))
  (parse-float:parse-float str :type 'double-float))

(defun to-keyword (str)
  "Transform that converts strings to (interned) keywords.
Note that no changes are made to string case.  If you want to coerce the case
for use by the transform (since transforms don't nest), you could use the 
:UPPER or :LOWER options to PARSE, e.g.

    (parse :upper \"abc\" ((keyword x))) => :ABC"
  (declare (string str))
  (intern str :keyword))

(defparameter *builtin-transform-functions*
  (list (cons :UPPER 'string-upcase)
        (cons :LOWER 'string-downcase)
        (cons :SNAKE 'to-snake)
        (cons :KEBAB 'to-kebab)
        (cons :LTRIM 'left-trim)
        (cons :RTRIM 'right-trim)
        (cons :TRIM 'trim)
        (cons :INTEGER 'parse-integer)
        (cons :FLOAT 'to-float)
        (cons :DOUBLE 'to-double)
        (cons :KEYWORD 'to-keyword))
  "Association list keyed by transform operator keywords (e.g. :UPPER)
and valued by function designators for the functions to perform the associated transform.")
        
(defun options-declare-symbol-pre-bound (options symbol)
  "If the PARSE options in the OPTIONS list have a :USING or :USING-VECTOR
declaration for SYMBOL, return the option name (:USING or :USING-VECTOR).
Otherwise return nil."
  (loop for option in options
        as  keyword = (and (listp option)
                           (member (car option) '(:USING :USING-VECTOR))
                           (car option))
        when (and keyword (member symbol (cdr option)))
        do (return keyword)))

(defun plain-symbol-p (s-exp)
  "Return true if s-exp is a symbol, but not a keyword."
  (and (symbolp s-exp) (not (keywordp s-exp))))

(defun binding? (s-exp)
  "Return true if s-exp represents a binding, NIL if it does not.
This applies to '_' symbols too.

For an expression to be a binding it must be either a symbol, or a valid transform expression."
  (or (plain-symbol-p s-exp)
      (and (listp s-exp)
           (or (string-equal (car s-exp) "TRANSFORM")
               (member (car s-exp) *transform-names* :test #'string-equal)))))

(defstruct (binding (:constructor %make-binding))
  sexp                   ;original macro argument, symbol or list
  null-p                 ;if symbol name is '_'
  previously-bound-p     ;true if (:USING x) or (:USING-VECTOR x) is in effect for symbol
  vector-p               ;true if (:USING-VECTOR x) is in effect for symbol
  shadow-symbol          ;gensym if no parse body & vector-p is true
  user-symbol            ;sexp if symbol, else second element of transform list
  transform-operator     ;NIL or keyword describing a transformation, e.g. :UPPER, :TRANSFORM
  transform-function)    ;built-in or user-defined function designator

(defun make-binding (options s-exp)
  "Given a list of parse options and an s-expression representing a symbol to be
bound, or list with a transformation expression having a symbol to be bound, return a
BINDING structure representing the binding.

Signals a condition if the binding expression has improper syntax,
assumes transform names have previously been validated by the call to `binding?`."
  (let* ((listp (listp s-exp))
         (tokens (if listp (length s-exp) 1))
         (user-symbol (if listp (second s-exp) s-exp))
         (transform-symbol (alexandria:if-let 
                               ((s (and listp (first s-exp))))
                             (and (or (string-equal s "TRANSFORM")
                                      (member s *transform-names* :test #'string-equal))
                                  s)))
         (transform-operator (and transform-symbol
                                  (intern (symbol-name transform-symbol) :keyword)))
         (user-defined-transform-p (eq transform-operator :transform))
         (transform-function-designator (and user-defined-transform-p (third s-exp)))
         (null-p (string= user-symbol "_"))
         (previously-bound-p (options-declare-symbol-pre-bound options user-symbol))
         (vector-p (eq previously-bound-p :using-vector)))
    (when transform-symbol
      (if user-defined-transform-p
          ;; user-defined transform via TRANSFORM
          (unless (and (= 3 tokens)
                       (symbolp user-symbol)
                       ;; The function designator forms are UNEVALUATED MACROEXPANSION ARGS
                       ;; so #'x and 'x are (function x) and (quote x)
                       (or (stringp transform-function-designator)
                           (symbolp transform-function-designator)
                           (and (consp transform-function-designator)
                                (member (car transform-function-designator)
                                        '(quote function lambda)))))
            (error "Expected (TRANFORM <symbol> <function-designator>), received ~s" s-exp))
          ;; predefined transform
          (unless (and (= 2 tokens)
                       (symbolp user-symbol))
            (error "Expected (<transform> <symbol>), received ~s" s-exp))))
    (%make-binding :sexp s-exp
                   :null-p null-p
                   :previously-bound-p previously-bound-p
                   :vector-p vector-p
                   :user-symbol user-symbol
                   :transform-operator transform-operator
                   :transform-function
                   (and transform-symbol
                        (if user-defined-transform-p
                            transform-function-designator
                            (or (aget transform-operator *builtin-transform-functions*)
                                (error "Internal error looking up built-in transform function for ~s" 
                                       transform-operator)))))))

(defun transform-function-sexp (binding)
  "Given a binding, return the transform-function as an s-expression suitable for a macroexpansion
environment.  Maybe I shouldn't need to do this but I didn't know another way."
  (alexandria:when-let ((tf (binding-transform-function binding)))
    (etypecase tf
      (symbol `',tf)
      (function tf)
      (list (third (binding-sexp binding))))))

(defparameter *null-binding*
  (make-binding NIL '_)
  "Binding indicating no variable should be bound.")

(defun template-bindings-and-patterns (options template)
  "Given PARSE options (already parsed list of keywords or lists starting with
keywords) and the TEMPLATE, an unevaluated and potentially empty list passed to
PARSE, return an association list whose keys are the binding (structures) in
template, or a binding for the symbol '_ if there isn't one for a pattern, and whose
values are patterns in the template, with possible pseudo end :END-OF-STRING-PATTERN.

Adjacent bindings will have a space pattern added between them.
If the template does not end with a pattern, :END-OF-STRING-PATTERN is used.

Nothing about the binding or pattern expressions is particularly validated yet.
We let `LET` in the PARSE expansion validate symbol names to bound (it will hopefully
complain about attempts to bind NIL).

Note that valid bindings may be lists whose second element is the symbol to be bound,
(the first element is the name of a TRANSFORM), e.g. (UPPER foo).
Note that valid patterns-as-s-exps may be lists whose car is `$`."
  (loop with result = nil               ;ALIST of symbol . pattern pairs.
        with last-binding = nil         ;haven't seen a new binding yet
        for sexp in template
        do (cond ((pattern? sexp)       ;emit current binding/pattern pair
                  (setf result (acons (or last-binding *null-binding*)
                                      (make-pattern sexp) result)
                        last-binding nil))

                 ((binding? sexp)
                  (when last-binding    ;consecutive bindings, word-split semantics
                    (setf result 
                          (acons last-binding (make-pattern :word-split-pattern) result)))
                  (setf last-binding (make-binding options sexp))) ;buffer current binding
                 
                 (t (error "Invalid pattern or binding expression: ~s" sexp)))
        finally 
           (when last-binding  ;binding is last template token, matches end-of-string
             (setf result 
                   (acons last-binding (make-pattern :end-of-text-pattern) result)))
           (return (nreverse result))))

(defvar *options* nil
  "This special variable is bound to a list of options in effect a PARSE invocation.
It is intended as a way for user extensions to access options they need to act upon
(e.g. CASELESS comparisons).")
  
(defun symbols-to-return (bindings)
  "Given a list of bindings, return all those variables that we will return if the parse BODY
is NIL. This includes regular PARSE allocated bindings based on template symbols,
as well as :USING symbols.  However it does NOT include vectors declared with :USING-VECTOR,
but instead will include shadow symbols allocated if there is no BODY."
  (loop with result = ()
        for binding in bindings
        unless (binding-null-p binding)
        do (pushnew (if (binding-vector-p binding)
                        (binding-shadow-symbol binding)
                        (binding-user-symbol binding))
                    result
                    :test #'string=)
        finally (return (nreverse result))))

(defun symbols-to-allocate (bindings body)
  "Given a list of binding structs, return symbols for those bindings which are not null
and that the user has not indicated LET bindings should be omitted.

If BODY is non-nil, that's all we need to do.  However if BODY is NIL and there are user-allocated
vector bindings, we need to allocate a shadow variable to generate the customary 
PARSE return value.  See the README.md on :USING-VECTOR for details."
  (let ((result ()))
    (loop for binding in bindings
          when (and (not (binding-null-p binding))
                    (not (binding-previously-bound-p binding)))
            do (pushnew (binding-user-symbol binding) result :test #'string=))
    (unless body
      (loop for binding in bindings
            when (binding-vector-p binding)
            do (let ((sym (gensym "PARSE-VECTOR-SHADOW-")))
                 (setf (binding-shadow-symbol binding) sym)
                 (push sym result))))
    (nreverse result)))

(defun parse-aux (options source template body)
  "Generate the macroexpansion for PARSE."
  (let* ((binding-pattern-alist (template-bindings-and-patterns options template))
         (bindings (mapcar #'car binding-pattern-alist))
         (patterns (mapcar #'cdr binding-pattern-alist))
         (source-idx-sym (gensym "PARSE-SOURCE-IDX-"))
         (last-match-start-sym (gensym "PARSE-LAST-MATCH-START-IDX-"))
         (match-start-sym (gensym "PARSE-MATCH-START-IDX-"))
         (next-idx-sym (gensym "PARSE-NEXT-IDX-")) ;is also matched region end idx
         (result-sym   (gensym "PARSE-RESULT-"))
         (source-sym   (gensym "PARSE-SOURCE-"))
         ;; symbols-to-allocate are those symbols for which we will create LET bindings
         ;; Allocates shadow symbols for vector-p bindings if no body
         ;; Note: side effect allocates shadow symbols for vector  bindings if necessary
         ;; Should probably have been done at make-binding time, but wasn't.
         (symbols-to-allocate (symbols-to-allocate bindings body))
         ;; All bindings except null ('_') bindings. Vector bindings are replaced by shadow symbols
         ;; Relies on shadow-symbols previously established for bindings
         (symbols-to-return (symbols-to-return bindings))
         match-forms source-transforms var-initforms)
    (setq match-forms 
          (mapcar (lambda (binding pattern)
                    (let ((symbol (binding-user-symbol binding))
                          decl match-sexp unmatch-sexp extract-p)
                      (if (binding-null-p binding)
                          ;; "_" case, explicitly or implicitly, no bindings to update
                          (setq decl `((declare (ignore ,result-sym)))
                                match-sexp `(setq ,source-idx-sym ,next-idx-sym
                                                  ,last-match-start-sym ,match-start-sym)
                                unmatch-sexp `(return))
                          ;; bindings are in effect
                          (let* ((xform (if (binding-transform-function binding)
                                            `(funcall ,(transform-function-sexp binding)
                                                      ,result-sym)
                                            `,result-sym))
                                 (assign-form (if (binding-vector-p binding)
                                                  (if (binding-shadow-symbol binding)
                                                      `(vector-push 
                                                        (setq ,(binding-shadow-symbol binding) ,xform)
                                                        ,symbol)
                                                      `(vector-push ,xform ,symbol))
                                                  `(setq ,symbol ,xform))))
                            (setq match-sexp `(progn
                                                ,assign-form
                                                (setq ,source-idx-sym ,next-idx-sym
                                                      ,last-match-start-sym ,match-start-sym))
                                  unmatch-sexp `(progn ,assign-form
                                                       (return))
                                  extract-p t)))
                      `(multiple-value-bind (,result-sym ,match-start-sym ,next-idx-sym)
                           (match-and-extract ,source-sym ,source-idx-sym ,last-match-start-sym
                                              ,pattern ,(pattern-argument pattern) ,extract-p)
                         ,@decl
                         (if ,next-idx-sym
                             ,match-sexp
                             ,unmatch-sexp))))
                  bindings patterns))
    (loop for option in options
          as transform = (case option
                           (:upper `(setq ,source-sym (string-upcase ,source-sym)))
                           (:lower `(setq ,source-sym (string-downcase ,source-sym))))
          when transform
            do (push transform source-transforms))
    (setq var-initforms 
          `((,source-idx-sym 0) 
            ,match-start-sym
            (,last-match-start-sym 0)
            ,next-idx-sym
            ,result-sym
            (,source-sym ,source)
            (*options* ',options)
            (*string-equality-predicate* ,(if (member :caseless options) 
                                              '#'string-equal ;note leading quote requirement here
                                              '#'string=))
            ,@(mapcar (lambda (symbol) 
                        (list symbol `*unmatched-binding-value*))
                      symbols-to-allocate)))
    (setq body (or body `((list ,@symbols-to-return))))
    `(let (,@var-initforms)
       (declare (ignorable ,source-idx-sym ,next-idx-sym
                           ,match-start-sym ,last-match-start-sym
                           ,result-sym ,source-sym 
                           ,@symbols-to-allocate) 
                (fixnum ,source-idx-sym)) ;also last-match-start-sym?
       (check-type ,source-sym string)  ;condition you can fix
       (locally (declare (string ,source-sym))
         ,@source-transforms
         (block nil ,@match-forms)
         (locally ,@body)))))

(defun coalesce-using-options (options)
  "Given a list of options as per `parse-options` but after `:using x` has been
transformed to `(:using x)`, if there are multiple :USING or :USING-VECTOR
directives, merge them into a single k/v pair and remove duplicate symbols that may
exist. Return the potenitally revised OPTIONS list. E.g.

(:upper (:using a b c) (:using a d e)) => (:upper (:using a b c d e))"
  (flet ((using? (x) (and (listp x) (eq (car x) :using)))
         (using-vector? (x) (and (listp x) (eq (car x) :using-vector)))
         ;; ((:using x y) (:using y z)) => ((:using x y z))
         (merge-using (usings)
           (let ((n-usings (length usings)))
             (cond ((= 0 n-usings) nil)
                   ((= 1 n-usings) (list (remove-duplicates (first usings))))
                   (t (list (cons (first (first usings)) ;:using, :using-vector
                            (remove-duplicates 
                             (apply #'concatenate 'list (mapcar 'cdr usings))))))))))
    (let ((usings (remove-if (complement #'using?) options)) ;the :using options
          (using-vectors (remove-if (complement #'using-vector?) options)) ;:using-vector options
          (non-using (remove-if #'(lambda (x) (or (using? x) (using-vector? x))) options)))
      (concatenate 'list (merge-using usings) (merge-using using-vectors) non-using))))

(defun parse-options (args)
  "Given the argument list to PARSE, return a list of validated options in the form of
either the option keyword for value-less options, or in the form of a list
whose car is the option keyword, and the rest of the list being the values of the options.

Return two values, the options list, and the cons in ARGS following the options
(whose CAR would be the SOURCE argument).

E.g.  `(parse :UPPER :USING (a b) \"abc\" (d e))`
      => (:UPPER (:USING a b))
         (\"abc\" (d e))
"
  (labels ((token-keyword (token)
             (cond ((keywordp token) token)
                   ((and (listp token) (keywordp (car token)))
                    (car token))))
           (validate (options)
             (when (and (member :upper options)
                        (member :lower options))
               (error ":UPPER and :LOWER options are mutually exclusive."))
             (let ((a (cdr (find :using options :key #'token-keyword)))
                   (b (cdr (find :using-vector options :key #'token-keyword))))
               (when (or (find '_ a :test #'string=)
                         (find '_ b :test #'string=))
                 (error "The null binding symbol '_ is not supported in :USING or :USING-VECTOR declarations."))
               (alexandria:when-let ((i (intersection a b)))
                 (error "These symbol~P were listed in both :USING and :USING-VECTOR declarations, and such declarations are mutually exclusive: ~s"
                        (length i) i)))
             options)
           (validate-symbols (using-list)
             (unless (every #'plain-symbol-p (rest using-list))
               (error "~s expects a list of non-keyword symbols, got ~s instead."
                      (car using-list)
                      (cdr using-list)))
             using-list))
    ;; Accept :USING X, or (:USING X).  Currently not supported
    ;; for case keywords, we'd have to change callers to check for lists.
    (loop with result = ()
          with cons = args
          as token = (car cons)
          as token-keyword = (token-keyword token)
          while token-keyword
          do (case token-keyword
               ((:upper :lower :caseless) 
                (push token-keyword result)
                (setf cons (cdr cons)))
               ((:using :using-vector)
                (cond 
                  ((listp token)        ;input was (:using[-vector] x)
                   (push (validate-symbols token) result)
                   (setf cons (cdr cons)))
                  (t                    ;input was hopefully :using (x)
                   (let ((val (cadr cons)))
                     (unless (listp val)
                       (error "Expected a list as the value of the ~s option, received ~s"
                              token val))
                  (push (validate-symbols (cons token val)) result)
                  (setf cons (cddr cons))))))
               (t (error "~s is not a valid PARSE option." token)))
          finally 
             (return (values (validate (coalesce-using-options result)) cons)))))

(defmacro parse (&rest args)
  "PARSE [option ...] source template [body]
Match text literals (or more complex patterns) in TEMPLATE against the text in
SOURCE, and bind the text which exists _between_ the matched literals to variables
named in the template.  BODY is then executed with the bound variables.  If no BODY
is specified, the effect is as if all bound variables are returned in a list in order
of their binding.

If there are more variables than text to match, unused variables are bound to 
*UNMATCHED-BINDING-VALUE* which defaults to REXXPARSE:+EMPTY-STRING+.

If there is more text than variables to match it, the last variable is bound to all
remaining text and there is no trimming of spaces. When a match for a pattern cannot
be found, it matches the end of the string.

Variables without patterns between them have an implicit word-split behavior, which
isn't quite the same as scanning for a single space pattern.

Patterns without variables between them discard the matched text, but each pattern
has is evaluated for its impact on advancing the position within the source string.

Example:

  (parse \"2024-JAN-12: George Washington slept here.\"
    (year \"-\" month \"-\" day \":\" first last did-what)
    (format t \"Yoda says: ~a ~a ~a did, ~a of ~a ~a, it was.~%\"
      did-what first last day month year))

   Yoda says: slept here George Washington did, 12 of JAN 2024, it was.
   => NIL

Supported options include:
:UPPER -- source string will be uppercased before parsing.
:LOWER -- source string will be lowercased before parsing.
:CASELESS -- character comparisions will use CHAR-EQUAL instead of CHAR=
:USING (vars) -- vars in list will not have binding allocation in macroexpansion
:USING-VECTOR (vars) -- like :USING, but vars must name fill-pointered vectors and will be
                        assigned using VECTOR-PUSH.

See README.md for examples of more complex matching behaviors as well as the ability
to transform matched text before binding."
  (let (source template)
    (multiple-value-bind (options body)
        (parse-options args)
      (setq source (or (first body)
                       (error "Missing SOURCE argument."))
            body (cdr body))
      (setq template (first body)
            body (cdr body))
      (unless (listp template)
        (error "TEMPLATE must be a list, not a ~s." (type-of template)))
      (parse-aux options source template body))))

#|
- Might be nice to accept `:using[-vector] x` as well when there's only one var (no list required)

- Allow characters as pattern literals too?
|#
