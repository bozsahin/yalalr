# yalalr
yet another LALR parser generator interface

This is an interface to <a href="http://web.science.mq.edu.au/~mjohnson/code/lalrparser.lisp">Mark Johnson's LALR parser generator</a>.

What it does in Common Lisp:

<<<<<<< HEAD
1. returns unknown tokens as type ID, so the lexicon is open-ended. The return value is (ID text), where text is the token itself.
2. provides a standard workflow for writing syntax-directed LR translation and code generation.
3. uses <code>lalrparser.lisp</code> as is. It is not part of this repo.
4. It can take input online or as output from a lexical analyzer (default case)
=======
1. Returns unknown tokens as things with the type <code>`ID'</code>, so the lexicon is open-ended, unlike the LALR parser. The return value is <code>(ID text)</code>, where text is the token itself. (A token is anything that the Common Lisp reader considers a token.)
2. Provides a standard workflow for writing syntax-directed LR translation and code generation.
3. Uses <code>lalrparser.lisp</code> as is. It is not part of this repo.
>>>>>>> 28cfd0bdb94985a5fbbd098b903d070066ae3630

soon.
-cem bozsahin
