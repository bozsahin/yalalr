# yalalr
yet another LALR parser generator interface

This is going to be an interface to <a href="http://web.science.mq.edu.au/~mjohnson/code/lalrparser.lisp">Mark Johnson's LALR parser generator</a>.

What it does in Common Lisp:

1. returns unknown tokens as things with the type <code>`ID'</code>, so the lexicon is open-ended, unlike the LALR parser. The return value is <code>(ID text)</code>, where text is the token itself.
2. provides a standard workflow for writing syntax-directed LR translation and code generation.
3. uses <code>lalrparser.lisp</code> as is. It is not part of this repo.

soon.
-cem bozsahin
