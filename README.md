# TL;DR Purpose

A DSL to concisely scan/tokenize, extract, and transform semi-structured string data, and
bind the results to variables. Inspired by the REXX PARSE command.

Some simple if not particularly inspired examples:

    (parse "The quick brown fox" (_ _ color animal)
      (format t "The color of the ~a is ~a~%" animal color))

    => The color of the fox is brown

    (defvar *log-line* "2024-Aug-12: [ERROR] Some stupid log error")
    (parse *log-line* (year "-" "[" severity "] " rest)
      (when (string= severity "ERROR")
        (format t "WARNING WILL ROBINSON! ~s happened in ~a~%" rest year)))

    => WARNING WILL ROBINSON! "Some stupid log error" happened in 2024

    (parse "Meal total: $23.12" ("$" (float dollars))
        (format t "Amount with 15 & 20 percent tips: $~,2f, $~,2f~%"
            (* dollars 1.15) (* dollars 1.20)))

    => Amount with 15 & 20 percent tips: $26.59, $27.74

# About

A long time ago there was a novel scripting language that ran on the IBM
VM/CMS operating system known as [REXX](https://www.rexxla.org/).

One of the things I always liked about REXX in its era of pre-regexp
scripting languages was the `PARSE` statement. In its simplest form PARSE
is a very nice way to parse strings with delimited or positional data and
then bind the matching substrings to variables.

This package attempts to reproduce REXX's `PARSE` statement as a Common Lisp DSL.

There's a bit of a zen thing to `PARSE`. Pattern matching is almost
the opposite of a regexp. Instead of specifying patterns for what you want
to match, you specify patterns for the bits that are not of interest, and
what gets bound as a match is the stuff in-between those uninteresting bits.

For example `(parse "12:00:00" (hh ":" mm ":" ss)) => ("12" "00" "00"))`
is matching for the colons, the other tokens are just variable names to
receive the text matched _around_ the tokens (though this example doesn't
show the variables in use, `PARSE` defaults to returning a list of the matched
variables if there is no body).

`PARSE` is about scanning strings and binding desired subsequences
to variables in the style of REXX.  It is not intended for lisp syntax
parsing, nor will it replace regexps where you need need to express complex
patterns. It is better suited for word-splitting and fixed-format data.

That said `PARSE` can sometimes express some parsing tasks in a clearer and
shorter way, and has other useful capabilities such as its ability to
act like a programmable tape reader (e.g. reading a length descriptor
from the input and then extracting a substring for the specified length).

Realistically this package probably adds little to what you can piece
together with other lisp parsing and/or pattern-matching packages, but if
you liked REXX, then perhaps you'll like this macro with its style of
binding and pattern specifications. This package is also regexp-free by
design. Some overlapping regexp capabilities are mentioned below.

# Tested platforms

Tests ran on the following without issues except as otherwise noted.

* SBCL
* ECL
* CCL
* ACL
* LISPWORKS
* ABCL - with the following mild warning about the `parse-float` packages
  when loading, but otherwise okay:

    ; Loading "rexxparse-test"
    ; Caught BAD-SYSTEM-NAME:
    ;   System definition file #P"/home/dave/quicklisp/dists/quicklisp/software/parse-float-20200218-git/parse-float.asd" contains definition for system "parse-float-tests". Please only define "parse-float" and secondary systems with a name starting with "parse-float/" (e.g. "parse-float/test") in that file.
    ; Compilation unit finished
    ;   Caught 1 WARNING condition


# REXX Compatibility

I have endeavored to make the basic string and position parsing compatible
with REXX semantics. So on the very slim chance you're a former REXX
programmer using Lisp, hopefully you will feel at home.

This was also the hardest part of the project because the REXX semantics
are sometimes subtle. I was never particularly knowledgeable of REXX to
begin with, and the REXX documentation is a bit hit-or-miss on some
details.  At times I was guessing at black box behavior. I've tried to boil
down the main rules in a section labled "Parse Rules 101" below.

Among the REXX compatibility features of REXXPARSE is tolerance of edge
cases like rebinding the same variable multiple times, position patterns
which are out of bounds of the string, and so on.  About the only
restriction is that relative position fixnums must not be negative, which
is in keeping with REXX semantics. Otherwise it tries not to complain about
mundane things in your templates.

If you think you've found a bug, try your PARSE with Object Object REXX and
see what it does. My goal is to match its semantics for any functionality
shared between the two, however note that the Lisp version has additional
capabilities which can't be compared.

# Alternative text parsing packages

Lisp has plenty of great tools that already do parsing, here's a couple for
consideration.

## cl-ppcre

If regexps are your thing you could also use the 
[cl-ppcre](https://edicl.github.io/cl-ppcre/#register-groups-bind)
`register-groups-bind` construct.  It probably performs just
as well (or better with its years of fine tuning, I have no idea). It even has its own
flavor of transforms that can be applied to the match before binding.

## scanfcl

There's also the Common Lisp [scanf](https://github.com/splittist/scanfcl)
tool, which provides a lisp equivalent to the C `scanf` family of functions
and has the ability to parse numbers for you, but does not provide bindings
and suffers from broader limitations of `scanf`'s parsing capabilities.

# Example comparison of regexp/scanf/PARSE

Here is an example of parsing a simple text string with regexps and/or
`scanf`, followed by the way parsing is done with `PARSE`.

Let's use this text string that we want to parse, where we want to 
tease out the year/month/day and error message components:

    (defvar *text* 
      "2024/02/23 17:35:42.022 -  unable to locate '/usr/local/examples/' directory")

Note the additional blank space after the hyphen as well.

## Using cl-ppcre `register-groups-bind`

    (cl-ppcre:register-groups-bind (year month day error-msg)
        ("(\\d+)/(\\d+)/(\\d+).* -  (.*)" *text*)
      (list year month day error-msg))

    => ("2024" "02" "23" "unable to locate '/usr/local/examples/' directory")

Nice enough, with the usual cross-eyed issues of writing regexps.

## Using `scanf`

    (scanfcl:sscanf *text* "%d/%d/%d %*s -  %s")

    => (2024 2 23 "unable") 

Scanf is nice because it will convert matched text to numeric types,
`REXXPARSE:PARSE` can do that as well via transforms, a REXXPARSE extension
to basic REXX capabilities.

Note that the scanf example is able to suppress scanning of some text with
the '*' modifier, but fails to parse the message that was desired with
whitespace content. You can use fixed width %s or %c if you could make
assumptions about the width but not generally compatible with most service
log content.  If your scanf supports character sets, you could use that
too. Still, it isn't super friendly for reading delimited substrings the
way we do with PARSE.

## Using `PARSE`

### Pure REXX PARSE

The original (NOT LISP!) REXX syntax would be:

    PARSE *text* year "/" month "/" day . "-" error

In the above statement, `*text*` is known as the source (to be matched),
and the remainder of the statement is known as the "template".  In REXX,
the period was a placeholder, in lisp we use '_' (underscore) because periods
have different behavior with the Lisp reader.  In the above example, the
period would match the timestamp text.

The template contains symbols naming variables to be bound, and strings to
be matched in the source text such that they delimit the text of interest
to be bound.

### Lisp-styled REXX PARSE

The general syntax of PARSE is

    (parse <source-string> (<template-elements>) <optional-body>)

The body allows for optional declarations of template variable symbols via
an implicit enclosing `locally`. Normally they will be strings unless you
are using transforms, but no such implicit declarations are made.

Here is a simple text parse without a body. The underscore is as mentioned above:

    (parse *text* (year "/" month "/" day _ "-" error))

If all you want to do is return a list of values bound, you can omit all forms 
after the template and a list of bound values will be returned, so the above
would return

    => ("2024" "02" "23" "unable to locate '/usr/local/examples/' directory")

Values are returned in order of the variables specified in the template.
Text conceptually (but not physically) bound do the placeholder `_` 
is not included in the result.

One of the main points of PARSE is to lexically bind variables for you
so you don't have to go and fetch them from a list with `destructuring-bind`
or other tools. For example:

    ;; mock snippet dealing with some error noted in *text*
    (parse *text* ("unable to locate '" path "' directory")
      (cerror "Create the directory ~s and continue"
              "The directory ~s did not exist" 
              path))

    =>

    The directory "/usr/local/examples/" did not exist
       [Condition of type SIMPLE-ERROR]

    Restarts:
     0: [CONTINUE] Create the directory "/usr/local/examples/" and continue
     1: [RETRY] Retry SLIME REPL evaluation request.
     2: [*ABORT] Return to SLIME's top level.
     3: [ABORT] abort thread (#<THREAD tid=88446 "repl-thread" RUNNING {10084300A3}>)

#### Template variables, bindings vs. assignment

Symbols acting as variables in the template, except for '_', are _bindings_
introduced by `LET` and initialized with
`REXXPARSE:*UNMATCHED-BINDING-VALUE*`.

However depending on the use of the symbols in the template, they may
undergo multiple assignments, either to text matched by the parse, or to
the result of transformations on the parsed text.

The '_' does not result in a binding, no `_` symbol is bound on the stack,
any template matches for this symbol will not be extracted or saved to any variable.

#### REXX variables vs. Lisp s-expressions

If you're reading REXX documentation (or otherwise familiar with it), such
as [Object Object REXX Reference](https://rexxinfo.org/reference/articles/oorexxref.pdf),
note that the use of parenthesized forms is different between REXX and
REXXPARSE:PARSE.  Where REXX would use a parenthesized expression to do a
language variable references, REXXPARSE uses parenthesized forms in templates for their
syntactic value beyond that, e.g. `(+ x)` to is a positional pattern to
move rightward `x` columns.  I imagine the confusion will only occur to
people who have been writing a lot of REXX recently.

### Word-oriented tokenization

The basic behavior of PARSE favors matching tokens delimited by
spaces. Absent specific patterns from you, the spaces around tokens bound
to variables are discarded.  Thus

    (parse "Now  is the time" (now is the-time))
    => ("now" "is" "the time")

Note the multiple spaces between "Now" and "is", all used to divide tokens
matched and discarded. This is different from a pattern indicating a space,
e.g.

    (parse "Now  is the time" (now " " is " " the-time))
    => ("now" "" "is the time")

Here 'is' is matched to the text between the point matched by the pattern
on the left and the point matched by the pattern on the right. The two
patterns match consecutive spaces and produce the a zero length binding.
Don't let it mess with your head too much, this is a fairly contrived example.

### More text than bindings

The last binding variable will be assigned any unmatched tail of the source
string.  E.g.

    (parse "a b c" (a b))
    => ("a" "b c")

In this situation, the text bound to the tail variable will not have spaces trimmed.

### More bindings than text

If there are unused variables because there are fewer words in the
source than there are variables in the template, unused variables will
be bound to `REXXPARSE:*UNMATCHED-BINDING-VALUE*`, which defaults to an
empty string (in keeping with REXX semantics).  You can change this
behavior by rebinding the variable.

    (parse "a b" (a b c))
    => ("a" "b" "")

### Consecutive bindings and/or patterns

Your template may have binding sequences without interleaved patterns, in which case
the implicit word splitting pattern applies.  It may also have pattern
sequences without interleaved binding variables, which may be useful if,
for example, you're looking to advance across like tokens, e.g.

    (parse "I want the text following the second occurrence of 'text', this text."
            ("text" "text" the-rest))
    => ("', this text.")

### Parse Rules 101

The simplest form of parsing template consists of a list of variable names.
The string being parsed is split up into words (characters delimited by
blanks), and each word from the string is assigned to a variable in
sequence from left to right. Leading blanks are removed from each word in
the string before it is assigned to a variable, as is the blank that
delimits the end of the word.

Beyond the simple case there are some rules to remember for the myriad
edge cases and features related to PARSE:

1. If there is one variable and no pattern, the variable matches the whole
   source string (no whitespace characters are removed).

2. If there are more variables than words, excess varables are bound to
   `*UNMATCHED-BINDING-VALUE*`. 

3. If there is more text than variables would match, the last variable is
   bound to all remaining text.  Sometimes called the "tail match" rule.
   Tail matches never eat spaces, they preserve the remainder of the source
   string to be matched.

4. [SUBTLE, CRUCIAL] Any explicit pattern (with a match in the source
   string) creates a logical break in the
   source string such the var to the left of the pattern is treated as a
   "tail match" situation on the substring terminated by the pattern.

   Moreover, variables to the left apply to the substring to the left
   of the pattern.  I.e.

   `(parse "a b c x g" (a b "x" g)) => ("a" " b c" " g")`

5. Where no pattern is given between two variables or between a variable
   and the beginning or start of the source string, an implicit "word
   splitting" takes place.  Word splitting eats spaces before a token to be
   matched, and one space after the token.

6. An empty string is never found, it always matches the end of the source
   string.  Specifying an absolute position of 1 as the pattern following a 
   variable has a similar effect as an empty string pattern, it leaves 
   the cursor positioned such that you can match source string again.

   `(parse " a b c " (a "" b)) => (" a b c " "")`
   `(parse " a b c " (a 1 b)) => (" a b c " " a b c ")`

7. Absolute positions less than one are treated as one.
   Absolute positions greater than the source string length are treated
   as being the string length.

8. Relative position expressions, e.g. `(- <exp>)` require the `<exp>`
   to be a non-negative fixnum or a string that can be converted to a
   non-negative fixnum.  Absolute positions expressed with `(= <exp>)` have
   the same rules.

9. Relative and absolute positional patterns are interchangeable _with one exception_.

   Normally template parsing of string matches skips the text matched by
   the string pattern.  However when a template sequence of the form
   `"string" variable <relative-positional>` does NOT skip the string
   pattern data when assigning to the variable, and so the pattern text
   will will appear in the variable.  For example (with non-relative examples too):
   
   `(parse " a b c " ("b" b)) => (" c "))`        ; "b" not included, no relative positional

    ;; '5' is effectively equal to the source scanning start position when its pseudo-pattern
    ;; is matched, which means an empty string match, which is a break/tail-position behavior.
   `(parse " a b c " ("b" b 5)) => (" c "))`      ; from end of "b" to 5 is empty, full break
   `(parse " a b c " ("b" b 7)) => (" c"))`       ; space past "b" to 7, 2 chars
   `(parse " a b c " ("b" b (+ 1))) => ("b"))`    ; from position of b to position+1
   `(parse " a b c " ("b" b (- 1))) => ("b c "))` ; from position of b to end of string

10. Template expressions may reference variables bound by preceding
    template matches. See the section on `Length Positional Patterns` for an example.

### Positional template directives

Patterns may also be positional directives, where integers specify absolute
or relative positions in the source string, relative positions being
relative to the start of the last pattern matched. Positions are generally
used for fixed length subfields in strings, but can also be used to re-scan
the source.

Like string patterns, positions identify points at which the source string
is split, only the length of the match is zero.  Also like string patterns,
variables bracketed by patterns will not be string trimmed.

    ;                1         2         3
    ;       1234567890123456789012345678901234
    (parse "Brimfield    Massachusetts   10101"
      (city 14 state 30 zip))
    => ("Brimfield    " "Massachusetts   " "10101")

_Absolute_ positions may be specified as positive integer literals.
The above example specifies position matches for columns 14 and 30.
Absolute positions are all 1-based integer values, i.e. a column ordinal. Subtract
one mentally for Lisp array indices. (This choice is for REXX compatibility).

For positions involving the integer-valued variables
instead of integer literals, you must supply an s-expression whose car is
one of `+`, `-`, `=`, followed by a s-expression that is evaluated at
runtime (not macroexpansion time) to produce an integer to be interpreted
as the relative or absolute position. The `+` and `-` expressions indicate
_relative_ positions, while `=` indicates an absolute position.

Examples:

    ;; City occupies columns 1-13 inclusive.
    ;; State occupies columns 14-29 inclusive
    ;; '+' indicates position relative to the prior pattern match position.
    (parse "Brimfield    Massachusetts   10101"
      (city (+ 13) state (+ 16) zip))
    => ("Brimfield    " "Massachusetts   " "10101")

    ;; Mixing absolute positions 30 and 31 with relative offsets.
    ;; reparsing the first '1' twice
    (parse "Brimfield    Massachusetts   10101"
      (30 one-a 31 (- 1) one-b (+ 1)))
    => ("1" "1")

    ;; use of variables must be through the parenthesized expression
    ;; otherwise they would be indistinguishable from variables to be bound.
    ;; '=' indicates absolute positions
    (defvar *state-column* 14)
    (defvar *zip-column* 30)
    (parse "Brimfield    Massachusetts   10101"
      (city (= *state-column*) state (= *zip*-column*) zip))
    => ("Brimfield    " "Massachusetts   " "10101")


Any positional directive that would precede the first source column
(i.e. are < 1) are treated as 1.

Any positional directive that would exceed the length of the
source string is treated as the string length, matching the
remainder of the string.

It is an error for any net position value to exceed the range of a fixnum.

### Positional pattern data types 

All positional expressions must be integers in the range of non-negative
fixnums, or s-expressions that resolve to those values. This constraint is
relaxed for `+`, `-`, and `=` patterns, as well as `>` and `<` (described
below) so that strings matched while parsing may be used later in the
template as numeric positional directives. Note that such uses 
of the value bound at one step of the parse act as input controlling later
steps of the parse.

Allowing strings as positional values is a shortcut to avoid the need that
for littering your template with `parse-integer` calls on previously
matched text.  String to fixnum conversions in positional templates that do
not resolve to non-negative fixnums will result in an continuable error
being signalled. Conversions are performed with `cl:parse-integer` and may
generate a `cl:parse-error` condition if the text is not not parseable as
an integer, and `parse-error` is not a continuable condition.

See the next section with examples matching integer data in the source
string and using those integers for subsequent match activity.

### Length Positional Patterns

A `length positional pattern` is a number in a `<` or `>` pattern sexp
similar to the `+`, `-`, and `=` pattern forms. I'm not sure why REXX
distinguishes this from `-` and `+` positional patterns, they are identical
in behavior except for one situation noted below.

As with `-` and `+` the number specifies the length at which the source
string is to be split relative to the current position. `>` and `<`
indicates movement right or left, respectively from the start of the string
or from the position of the last match.

The `>` length pattern and the `+` relative positional pattern are
interchangeable except in the special case of a zero value. A `(> 0)` pattern
will split the string into a null (empty) string and leave the match position
unchanged, whereas a `(+ 0)` pattern also leaves the match position
unchanged, but doesn't split the string.  In essence `(> 0)` says "match
empty string" whereas `(+ 0)` advance scan zero characters, matching
whatever follows.

This string splitting behavior is useful for parsing string subfields
whose lengths are also encoded in the string.

The following example shows the difference between `(> 0)` and `(+ 0)`,
note the different matches for `middle`:

     ;; Parsing with length patterns
     (parse "04Mark0005Twain" 
            (len (+ 2) first (> len) len (+ 2) middle (> len) len (+ 2) last (> len))
        (list first middle last len))
     => ("Mark" "" "Twain" "05")

     ;; Parsing with relative patterns only
     (parse "04Mark0005Twain" 
            (len (+ 2) first (+ len) len (+ 2) middle (+ len) len (+ 2) last (+ len))
        (list first middle last len))
    => ("Mark" "05Twain" "Twain" "05")

While `<` is similar to `-`, application of of the match/extract process
differs.  To achieve the effect of `<` on a region of text with `-` you
must use a `-`/`+` pair, and the position in source differs as in the
following example:

    ;; Parsing with length patterns
    (parse "12345.6789" ("." digit (< 1) rest)) => ("5" "5.6789")
    ;; Parsing with relative patterns
    (parse "12345.6789" ("." (- 1) digit (+ 1) rest)) => ("5" ".6789")

`<` is similar to matching a string literal, _without_ advancing the next
position to be scanned after binding.

### Transformations (REXXPARSE extension)

`REXXPARSE::PARSE` supports transformations on matched strings before they
are assigned to variables.  Transforms are a REXXPARSE lisp extension and
not part of the basic REXX PARSE capability.

The syntax for an assignmement based on a predefined transformation is:

    (<transform> variable)

where you would otherwise just have a variable to be bound that wasn't in a
list.  Note that the above uses `<transform>` as a non-terminal BNF token
representing many possible pre-defined transformations. There is also a `TRANSFORM`
terminal symbol with specific user-defined transformation semantics.

Transforms have the same syntax as list-form patterns but are in
fact binding forms. PARSE distinguishes patterns from transform-augmented
bindings by the symbol name of the CAR of the list being known as a
transform symbol.

Transforms are a convenience for common parse situations, you
could always do the transformations in the `&BODY` of the parse if you need
different transformation semantics than those pre-defined by REXXPARSE or
simply don't like the confusion transform syntax that resembles pattern syntax.

    (parse "some text with numbers: 1.0 2" (_ ": " (float x) (integer n))
      (format t "~f is a ~s, ~d is a ~s~%" x (type-of x) n (type-of n)))

    =>
    1.0 is a SINGLE-FLOAT, 2 is a (INTEGER 0 4611686018427387903)
    NIL

The `(float x)` and `(integer n)` expressions are DSL syntax to invoke transformations
on the text corresponding to variables `x` and `n`, and assigning the
transformation result to those variables.  The set supported
transformations are describe below.

Transformations do not currently nest, i.e. you _cannot_ do `(LOWER (KEBAB x))`
if you need to apply more complicated transformations, see 'user defined transforms' below.

Transforms expressions using the `_` symbol are effectively NO-OPs.  No
text is extracted, and no transform function is run.

#### Pre-defined transforms

* UPPER       - uppercases the extracted text.
* LOWER       - lowercases the extracted text.
* SNAKE       - convert hyphens to underscores.
* KEBAB       - convert underscores to hyphens.
* LTRIM       - remove leading spaces.
* RTRIM       - remove trailing spaces.
* TRIM        - remove leading and trailing spaces.
* INTEGER     - convert extracted text to an integer.
* FLOAT       - convert extracted text to a single-float.
* DOUBLE      - convert extracted text to a double-float.
* KEYWORD     - convert extracted text to a keyword.

The floating point conversions are done using the `:parse-float` package,
they do _not_ perform unsafe `READ`s. INTEGER conversion is done using `PARSE-INTEGER`.

`SINGLE-FLOAT` and `LONG-FLOAT` conversions are not supported, the
`:parse-float` package doesn't seem to support them on SBCL at least. If
you need these representations you'll probably want to use
`*READ-DEFAULT-FLOAT-FORMAT*` bindings with a user-defined transform that
observes it and manages the conversion.

Of course you could also just supply a BODY to the `PARSE` form and do the
conversions in the body, there's no need whatsoever to use user-defined
transforms except perhaps to abbreviate code if the transformation is used a lot.

The `LTRIM`, `RTRIM`, and `TRIM` transforms use the Common Lisp `STRING-LEFT-TRIM`,
`STRING-RIGHT-TRIM`, and `STRING-TRIM` functions respectively, supplying
`REXXPARSE:*TRIM-CHARACTER-BAG*` as the character bag argument. This is
exported so that you may bind it to other characters (outside of the
`PARSE` form), but note that it will
affect all trim transforms in the scope of the binding.

The KEYWORD transform does no conversions to case, so it's easy to make
symbols in unexpected cases if you aren't careful. If you want to
upper/lower case the text before the transform makes a keyword of it, you
could use `:UPPER` or `:LOWER` options to `PARSE` (though that will change
the case of the whole source string).  Or you can just do what you want in
the `PARSE` body.

#### User defined transforms

There is a special transform operator, `TRANSFORM`, which exists to invoke
user-supplied transformation functions.

    (transform <symbol> <function>)

Will invoke `function` on the text extracted by the parse, and assign the 
result of the transformation function to `symbol`.  `function` must be a
[function designator](https://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#function_designator)
for a function of one argument which will always be a string. 

    ;; User defined transform example
    (defun stupid (str) "Stupid!")
    (parse "Don't call me dull." (_ _ _ (transform s 'stupid) "."))
    => ("Stupid!")

## Full PARSE syntax

The general form of a `PARSE` invocation (pardon the weak BNF) is below.
Only the `<source>` expression is required, and it must yield a string.

    PARSE <options> <source> <template>

    <options> ::=
    <options> ::= :UPPER
    <options> ::= :LOWER
    <options> ::= :CASELESS
    <options> ::= :USING (<var> ...)
    <options> ::= (:USING <var> ...)
    <options> ::= :USING-VECTOR (<var> ...)
    <options> ::= (:USING-VECTOR <var> ...)

    <source> ::= string-literal
    <source> ::= s-exp

    <template> ::= 
    <template> ::= <template-expression>

    <template-token> ::= <binding>
    <template-token> ::= <pattern>

    <template-expression> ::= <template-token>
    <template-expression> ::= <template-expression> <template-token>

    <pattern> ::= string-literal
    <pattern> ::= <position>
    <pattern> ::= ( $ <s-exp> )

    <position> ::= position-integer-literal
    <position> ::= ( + <position-integer> )
    <position> ::= ( - <position-integer> )
    <position> ::= ( = <position-integer> )
    
    <position-integer> ::= position-integer-literal
    <position-integer> ::= <sexp>

    <binding> ::= symbol
    <binding> ::= ( <transformation> symbol )

    <transformation> ::= ( <built-in-transformation> symbol )
    <transformation> ::= ( TRANSFORM symbol function )
    
    ;; String producing case(like) transformations
    <built-in-transformation> ::= UPPER
    <built-in-transformation> ::= LOWER
    <built-in-transformation> ::= SNAKE
    <built-in-transformation> ::= KEBAB

    ;; Non-string producing transformations
    <built-in-transformation> ::= INTEGER
    <built-in-transformation> ::= DOUBLE
    <built-in-transformation> ::= FLOAT
    <built-in-transformation> ::= KEYWORD
    
Symbols are compared by name (so package doesn't matter), but upper case symbol names are expected.

1. `<source>` is evaluated to produce a string, unless it is already a string.
2. `<pattern>` is used to find the text region in `<source>` to be bound to
   the symbol in `<binding>`.
3. Once a region of text is matched by a pattern, it is assigned to the
   `<binding>` symbol. If there was no match or the source text is
   exhausted, the symbol is bound to `REXXPARSE:*UNMATCHED-BINDING-VALUE*`. 
   Do not mutate returned value. 
4. `<binding>` can also be an sexp of the form of the form `(TRANSFORM function symbol)`, 
   in which case `function` is run to transform the matched text before assigning it to `symbol`.

## `($ <s-exp>)`: variables (or other s-exps) as patterns

Patterns of the form `($ <s-exp>)` are used to indicate that the expression
`<s-exp>` is to be evaluated to produce a string to be used as a
pattern. It is needed because naked symbols in the template are interpreted
as binding names, and so are not normally evaluated.  `$` causes them to be
evaluated and treated as string-literal patterns, similar to a variable
evaluation directive in various other languages.

Example:

    (let ((x "brown"))
      (parse "the quick brown fox" (start ($ x) end)))
    => ("the quick " " fox")

The `$` form is only needed to evaluate symbols in patterns that don't
otherwise evaluate them. Positional patterns directives such as `(+ x)`
already evaluate their arguments. `(+ ($ x))` is not only unecessary, it
also wouldn't work (as `$` is a pattern directive, not a function for arbitrary
s-exp evaluation).

# Differences from REXX' PARSE 

## The Lisp bits

First of all, you're in lisp.  So PARSE is a DSL of a style similar to
Common Lisp's advanced LOOP macro.  If you don't like LOOP, you may not
like PARSE.

Second, there is an extensible mechanism you can use both to specify the
patterns matched in the template, and _transformations_ on the bound
values.

## PARSE UPPER|LOWER|CASELESS => PARSE :UPPER|:LOWER|:CASELESS

Parse options like UPPER are specified as keywords and not plain symbols.

## The source string can be any s-expression that yields a string.

REXX required a lot of additional syntax to decalre how the string
expression was interpreted, e.g. 'PARSE VAR'.  REXXPARSE does not suffer
from this, one source s-expression fits all so long as it yields a string.

NIL is not permitted for the source string.

## A missing template renders PARSE a NO-OP

As there is no `PARSE LINEIN` or `PARSE PULL` in this implementation,
if there is no template the `PARSE` invocation is a NO-OP, or as close to
it as we can make it.

If the template is missing or does not specify any variables other than
`_`, the `<source>` expression is not evaluated.

## There is no multi-string comma operator

The original REXX PARSE would bind multiple strings with a comma separating 
multiple templates. This is not supported.

## Positional Position Syntax

In REXX, a plus `(+)` or minus `(-)` before an integer indicated a
_relative_ position to be matched in the template. The presence of a `+`
was not the same as a positive value. A simple "10" indicates an absolute
position, whereas a "+10" indicates a relative position.

To do this in lisp requires that `+` be a separate token to survive the
reader. So we could represent a relative plus position as `(+ 10)`, `+ 10`
and so on.

Then there's the evaluation aspect if you're using an expression
value. E.g., you want to say `+myval` for some variable `myval`.  REXX 
would render that as +(myval) according to its syntactic semantics.

REXXPARSE:PARSE allows for the following for positional positions.

For shorthand positional patterns of constant values we allow integers and
keywords like these: `:+10`, `:-10`, `:=10`, `:10`. You can also use
integers, possibly negative, e.g. `10` and `-10`, but we cannot infer
relative positions on positive integers.

The long form syntax for positional patterns which might indicate
relativity as well as the value of arbitrary expressions is as follows
for some expression `x`, which may also be any number for which integer
coercion semantics are defined.

    (+ x)
    (- x)
    (= x)

## Additional PARSE options

REXXPARSE:PARSE allows for a number of options that change various
behaviors of the parse. Options are specified as (optional) keywords that
precede the source string expression to be parsed, i.e.

    (PARSE [:option1 ... :optionN] source-sexp (template) ...)

Some options have accompanying values, some do not, and some may be expressed
as lists.  All options, and only options, are triggered by keywords in the
PARSE arguments.

The following sections describe the options.  I have attempted to attribute
options to the REXX language versions that introduced them, please
provide corrections if the attributions are incorrect.

All options specified are bound to `REXXPARSE:*OPTIONS*` for the scope of
the PARSE expression, so that user-extensible patterns or other operators
can check for options that might require consideration, such as :CASELESS
comparisons. 

The traditional REXX options (:UPPER, :LOWER, :CASELESS) are plain
keywords that are _not accompanied by values_, their presence triggers the intended
behavior. Other options may accept values, refer to the documentation on
individual options for details.

### PARSE :UPPER

`PARSE :UPPER` converts lowercase a-z to uppercase before parsing. Note that
this represents a transformation (by copying) of the source string before parsing, 
and matched content will by definition be upper case as the source string
will no longer have any lowercase text.

    (parse :upper "A b C d" (w "C" r)) => ("A B " " D")

Note that specifying lower case string patterns will foil matching, 
:UPPER has no effect on the pattern text or the comparisons used.

    (parse :upper "A b C d" (w "c" r)) => ("A B C D" "")

`UPPER` was the only option in the original REXX PARSE construct.

The `:UPPER` option is mutually exclusive with the `:LOWER` option.

### PARSE :LOWER

`PARSE :LOWER` converts uppercase a-z to lowercase before parsing. Note that
this represents a transformation (by copying) of the source string before parsing, 
and matched content will by definition be lower case as the source string
will no longer have any uppercase text.

    (parse :lower "A b C d" (w "c" r)) => ("a b " " d")

Note that specifying upper case string patterns will foil matching, 
:LOWER has no effect on the pattern text or the comparisons used.

    (parse :lower "A b C d" (w "C" r)) => ("a b c d" "")

The `LOWER` was added by the NetREXXX language specification.

The `:LOWER` option is mutually exclusive with the `:UPPER` option.

### PARSE :CASELESS

`PARSE :CASELESS` ignores case on the comparisons. Unlike :UPPER and other
options it does not transform the source string, but instead changes the
character equality predicates used for comparison.

    (parse :caseless "A b C d" (w "c" r)) => ("A b " " d")

The `CASELESS` was added by the Open Object REXX language specification.

Note that this option may not be supported by pattern processors used as
extensions to the REXXPARSE behavior.  Extensions may implement the desired
behavior by examining the value of `REXXPARSE:*OPTIONS*` which is bound to
options specified in the PARSE form.

### PARSE NUMERIC (unsupported)

The IBM z/OS version of REXX supports a `parse numeric digits form fuzz`
packaging of the `numeric` operator.  This is not supported by REXXPARSE.

### PARSE :USING (<var> ...) or (:USING <var> ...)

The `:USING` option indicates that for all symbols in the var list,
`PARSE` should _not_ allocate bindings in its macroexpansion `LET` block,
and should instead use vars which already exist in the environment.
This may be useful for iterative performance or other application logic reasons.

When you request this behavior the vars do not undergo any initialization
step by `PARSE`.  If they are not assigned values by by the
match/extract/assign steps (because there are more variables than matches)
then whatever value they had going into parse is the the
value they will have in the body of `PARSE`.

Example:

    (let ((x nil)
          (y t))
      (parse :using (x y) "abc" (x)))
    => ("abc")

With the side effect that X is now "abc", and Y, which was not matched
or assigned, is still T.

### PARSE :USING-VECTOR (<var> ...) or (:USING-VECTOR <var> ...)

The `:USING-VECTOR` option is similar to the `:USING` vector. Symbols in
the list will not be allocated or initialized in the macro-expansion.

However in this case the symbols are expected to refer to fill-pointered
arrays when the PARSE macroexpansion is executed. Where an ordinary
`:USING` symbol would be assigned with `SETQ` or `SETF`, assignments to
symbols named by `:USING-VECTOR` will be executed by `(VECTOR-PUSH <value>
<var>)`. The variable is typically reused for multiple template bindings.

The array must exist and be large enough to accept the new value(s), and you
should ensure the fill pointer is where you want it on entry to `PARSE`.
It is an error if symbols named in `:USING-VECTOR` are also specified in
`:USING`.  Note that `VECTOR-PUSH` will not modify the array or signal a
condition if the fill-pointer indicates the array is full.

Aside from other possible performance or logic utility, the use of vectors
enables you to ask how many template assignments were matched and executed
by querying the fill-pointered vector length.  While you can query ordinary
`PARSE` bound vars to see if they were not matched, use of `:USING-VECTOR`
may be a more efficient way to get a count if the input is unlikely to
match bindings in a predetermined fashion.

Example:

    (let ((v (make-array 5 :fill-pointer 0)))
      (parse :using-vector (v) "abc def" (v v v)) ;=> ("abc" "def" "")
      v)
    => #("abc" "def")

Vector symbols in the template may not be used for input in positional
directives in the way that ordinary symbols are used. If you want to use a
value matched and stored in a vector in a previous template binding, in a
positional pattern you'll need to AREF your previously matched slot in the
positional pattern s-exp.  I.e.

    (let ((v (make-array 4 :fill-pointer 0)))
      (parse (:using-vector v) "04Mark" (1 v (+ 2) v (> (aref v 0)))
        v))

    => #("04" "Mark")

Some notes on return values when using vectors
_when there is no `&BODY`_ provided to the parse.

1. `PARSE` normally returns a list of all symbols values
   with binding specifications in the template, so it would normally return
   `("abc" "def" "")`.  However while there are three binding expressions
   in the example template above there are only two _assignments_ (only two
   matches) so the vector only receives two values.

2. `PARSE` endeavors to return values (again, when there isn't a BODY),
   such that the bound/assigned values are the same whether vectors or
   non-vectors variables are used in the template.

   This would be difficult with vector since we're neither initializing the
   vector, nor certain that `(aref v <n>)` has any meaningful value, or is
   even accessible if it wasn't assigned by the parse.  E.g.

    `(parse :using-vector (v) "a b" (v v v)) => ???`

   If you'd done (parse "a b" (a a a)) it would return `("")`, the last
   value matched for the only template binding symbol.
   
   For this reason, without a BODY specification we do extra setup for
   vectors and create shadow symbols for the bindings in the template that
   specify vectors. The shadow symbols are initialized and assigned like regular
   variables, solely so that we have something to make the PARSE default
   return value compatible with similar uses of non-vector template binding
   variables.  Thus the above example would return ("a" "b" ""), just as if
   you'd said (parse "a b" (a a a))

   Similarly, (parse :using-vector (v) "a b" (v b)) => '("a" "b")

3. You may wish to return NIL (or some other non-default value) to avoid
   `PARSE` consing a list as the default result (when there is no BODY)
   when you already have the result in a vector or other previously
   allocated bindings, presumably because you may be trying to avoid
   consing when you specify :USING or :USING-VECTOR.

   Of course this (NIL or other BODY return) also suppresses maintenance of
   the vector shadow symbol discussed in item 2 as well.

# User Extensible Behaviors

This section describes ways to extend REXXPARSE behavior by adding new
scanners (pattern matchers), transformers, and options (*TBD* - maybe not options).

# Future work

## Cleanup and code improvements

It took me longer than expected to grok how REXX' PARSE command works, and
my implementation took many twists and turns as I went down that road of
discovery.  The result is some code I don't like that could undoubtedly be
streamlined to express the rexx semantics better and work a bit faster.
In particular the scanners and the calling logic that decides what to do
with the scanner data, e.g. figuring out what is supposed to be extracted
with the semantics of '>' vs '+'. 

## Regexp patterns

The base `REXXPARSE` capabilities emulate REXX, and by design do not
incorporate regular expressions into the functionality.  However I was
thinking it would be nice to also allow that if people want it, in a
separate ASDF lisp system that combines CL-PPCRE and REXXPARSE into a
`REXXPARSE-RE` system, so that regexp string scanning is done via a
user-extensible interface to REXXPARSE. I.e. inaddition to plain string
literal patterns, you'd have regexp patterns as well. Note that this
wouldn't change the inverted matching style of `PARSE`, it would just
augment what could be matched.

It isn't clear that anybody will ever use REXXPARSE much less a
hypothetical REXXPARSE-RE, so this is unlikely to appear without an
indicator of interest.

## Some real use of the extension mechansims

The main tentative extension mechansims now are `*OPTIONS*` and
`*PATTERN->SCANNER*`.  I've chosen to use special variables and NOT generic
functions so that in the unlikely event you had two users of REXXPARSE in
the same lisp system, they could both extend the behavior without
clobbering each other, and generic functions would not support that.

However I haven't actually tested this, so consider the extension
mechansisms a work in progress until someone builds something like
`REXXPARSE-RE` below, or other things, to battle test the extension logic.
I.e. there may be breaking changes (only on the extension mechansims) until
I know it's usable.
