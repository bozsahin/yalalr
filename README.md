# yalalr
yet another LALR parser-generator interface

This is an interface to <a href="http://web.science.mq.edu.au/~mjohnson/code/lalrparser.lisp">Mark Johnson's LALR parser generator</a>.

The installer can download and install (1) lalrparser.lisp, (2) a suitable Common Lisp if desired, and (3) set itself up to be used from any directory. 

Just clone this repo and do <code>./install</code>.

What it does in Common Lisp:

1. returns unknown tokens as type ID, so the lexicon is open-ended. The return value is (ID text), where text is the token itself.
2. provides a standard workflow for writing syntax-directed LR translation and code generation.
3. uses <code>lalrparser.lisp</code> as is. It is not part of this repo.
4. It can take input online or as output from a lexical analyzer.

To use:

0. You need a lexical analyzer called 'lexer' and syntax-directed code generator 'sdd.lisp'. These are automatically loaded when begun.
1. <code>yalalr</code>
4. <code>(target-code x)</code> to generate code for <code>x</code>.
6. Use <code>(target-code-mips x)</code> if you want to generate MIPS code in <code>expr-mips</code>. This one clears
the symbol hash table before calling <code>target-code</code>.
7. <code>(help)</code> explains all this.


enjoy.
-cem bozsahin
