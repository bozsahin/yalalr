# yalalr
yet another LALR parser generator interface

This is an interface to <a href="http://web.science.mq.edu.au/~mjohnson/code/lalrparser.lisp">Mark Johnson's LALR parser generator</a>.

What it does in Common Lisp:

1. returns unknown tokens as type ID, so the lexicon is open-ended. The return value is (ID text), where text is the token itself.
2. provides a standard workflow for writing syntax-directed LR translation and code generation.
3. uses <code>lalrparser.lisp</code> as is. It is not part of this repo.
4. It can take input online or as output from a lexical analyzer (default case)

It can download and install lalrparser.lisp, a suitable Common Lisp, and set itself up to be used from any directory. Just clone this repo
and do <code>run-to-complete-first-time-install</code>.

-cem bozsahin
