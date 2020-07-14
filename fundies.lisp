; S-expressions (symbolic expressions) are either
;   an atom
;   an expression of the form (x . y) where x and y are both s-expressions

; A list is a collection of zero or more sexprs where the final element is the empty list

; Because a list can contain lists, a tree structure can be built out of lists. Ditto for sexprs

; A compiler turns a string into a stream of tokens, and the tokens into an AST.
; The AST is used to generate subsequent tree structures that ultimately become the instructions
; that are either executed (intepreter) or output (compiler)

; Since Lisp expressions are s-expressions, they are by definition tree structures already
; An Sexpr that can be evaluated is called a form: (+ 1 2) is a form, (1 2) is not. (Both are lists)

; The evaluator is passed a tree structure, so it has no need of a tokeniser or parser
; Macros are programs that take a list as input and generate a new list as output
; Macros have the full power of the native Lisp language to perform this task
; Because Lisp is *all* about processing lists (clue in the name!) this is *serious* power
; Because macros can do immensely powerful things with lists, and those lists are evaluated,
; Lisp macros give vast and reliable power to the language.

; e.g. the string "(+ (* 2 3) 4)" tokenises to something like

; o_par -> fun -> o_par -> fun -> num -> num -> c_par -> num -> c_par
;           +               *      2      3               4

; which generates the AST

;       Fun
;       add
;      /   \
;   Fun    Num
;  times    4
;   / \
; Num Num
;  2   3

; The string also evaluates to a list. Represented by cons pairs:

;       [ ][ ]
;       /   |
;      +    |
;           |
;          [ ][ ]
;         /     \
;      [ ][ ]   [ ][ ]
;      /   |    /    \
;     *    |   4     nil
;          |
;         [ ][ ]
;         /    \
;        2      [ ][ ]
;               /    \
;              3     nil

; Note similarity of structure between list and AST

; The compiler/interpreter is broken into two parts:
;   the reader - takes a string of input and parses it into an Sexpr
;   the evaluator - takes a list as input and evaluates it as a form

; Because both are available, it's trivially easy to create a REPL;
; but also to get a data structure from a string
; and to evaluate a data structure as code

; e.g. in perl you can
;   eval "say qq/Hello there/"
; and it'll run, sure. But your input is only ever a string and all you get out of it is the result
; of running that string as code. Nothing you can operate on.

; There's no access to anything in-between, to the AST or anything the compiler does
; It's a black box, string goes in, code gets run. The input can be transformed, yes,
; but only in limited textual ways - regexes etc. You'd have to build a parser or a string-generator
; yourself to get it to take an input string, transform it, and eval it

; In Lisp, you can take any input - a string from the user, data structures from a file, whatever
; (read) will extract an Sexpr from it. And you can take any existing list & manipulate
; that list with any & all of the tools Lisp comes with to do so, retaining all local environment etc.
; And *then* you eval your generated list.

; Because you have access to the read function, you can get past all edge cases like comments, strings etc.
; And because you have access to the eval function, you can send any list you like to be evaluated

; Conventional languages:
;                _____________
; source code -> | Black Box | -> code is interpreted/compiled
;  (strings)

; Conventional languages with preprocessor macros (C etc):
;                                _____________
; source code -> preprocessor -> | Black Box | -> code is interpreted/compiled
;  (strings)
; (Preprocessor has its uses but has totally different syntax and rules to the source language,
; and still works on text rather than data structures)

; Lisp
;
; source code -> (read) -> S-expression -> (eval) -> code is interpreted/compiled
;  (strings)                            ^
;                                       |
;                                      HERE
;                               is where the power is
;             The full functionality of Lisp is available to manipulate the Sexpr

; Additionally, the simple, uniform syntax of Lisp makes it easy to generate new code
; that will always behave in the desired fashion
; e.g. conditionals in other languages: if-else
; If you had an "if" function, it would be called like
;   if($test, $if_true, $if_false)
; But as a statement, it's
;   if ($test) { # handle truth} else { # handle false }
; And because it's a statement not an expression, you can't do "$foo = if ..."

; There's no uniformity: parens *and* curly brackets; requirement for an "else" keyword;
; the "if" is prefix but the test is likely to be infix: `if ($foo == $bar)`
; and often the if can be post-fix too: `$foo = $bar if $bar`

; Lisp is always prefix: (<verb> <args>) for *everything*
; There are no statements, everything is an expression.
;   (if test handle_truth handle_false)
; And because it's a statement, where other would have to do
;   if ($test) { print "True" } else { print "false" }
; Lisp can instead do
;   (print (if test "True" "False"))
; No need for redundant instructions, or an "else", etc.

; Because the syntax is so consistent, there's no handling of "This bit's prefix, then there's an infix,
; and we need to handle postfix options, and and and..." - a macro can make sweeping assumptions about
; the structure of the list it takes as input and the list it will output, simplifying the cases it
; must handle.

; Lisp types, like much of FP, should be thought of not via implementation details, but instead by
; how you interact with them.

; e.g. a C array is a contiguous area of memory accessed via offset:
;   foo[0] is the start because no offset needed to find it, etc. All makes sense

; A LISP list is made up of cons cells, which are created via (cons) and accessed via (car) and (cdr)
; They could as easily be made of functions via closure or by being a pair of pointers.
; You shouldn't know, or need to know, which (if either) they actually are.

; A LISP form can be:
; # a function
; # a macro
; # a special operator

