# yalalr
yet another LALR parser generator interface

This is an interface to <a href="http://web.science.mq.edu.au/~mjohnson/code/lalrparser.lisp">Mark Johnson's LALR parser generator</a>.

What it does in Common Lisp:

1. returns unknown tokens as type ID, so the lexicon is open-ended. The return value is (ID text), where text is the token itself.
2. provides a standard workflow for writing syntax-directed LR translation and code generation.
3. uses <code>lalrparser.lisp</code> as is. It is not part of this repo.
4. It can take input online or as output from a lexical analyzer (default case)

<b>Yacc-like</b> workflow: Write a grammar and generate LALR parser. Write a lexer, make sure it wraps the whole thing in (..), save it in 'tokens' file, and do <code>(target-code)</code> in yalalr to generate code.

It can download and install lalrparser.lisp, a suitable Common Lisp if desired, and set itself up to be used from any directory. Just clone this repo
and do <code>run-to-complete-first-time-install</code>.

-cem bozsahin
