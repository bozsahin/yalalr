# yalalr
yet another LALR parser-generator interface

This is an interface to <a href="http://web.science.mq.edu.au/~mjohnson/code/lalrparser.lisp">Mark Johnson's LALR parser generator</a>.

The installer can download and install (1) lalrparser.lisp, (2) a suitable Common Lisp if desired, and (3) set itself up to be used from any directory. 

Just clone this repo and do <code>./install</code>.

Some features of <code>yalalr</code>:

1. returns unknown tokens <code>x</code> with type ID, as <code>(ID x)</code>, so the lexicon is open-ended. 
2. provides a standard workflow for writing syntax-directed LR translation and code generation, using <code>(make-lalrparser)</code>
3. uses <code>lalrparser.lisp</code> as is. It is not part of this repo.


To use:

0. In your current directory, you need a lexical analyzer called <code>lexer</code> and syntax-directed code generator called <code>sdd.lisp</code>. These are automatically loaded when begun. Do:
1. <code>yalalr</code>  from the command line. Once you are in and everthing is ok:
2. <code>(target-code x)</code> to generate code for source file <code>x</code>. Alternatively you can call <code>ic-gen</code>
for IC generation. They call the lexer to obtain <code>x.tokens<//code>, then generate code.
2'. Use <code>(target-code-mips x)</code> if you want to generate MIPS code in <code>expr-mips</code>. This one clears
the symbol hash table before calling <code>target-code</code>.

You will get a warning in the beginning if the <code>lexer</code> or the <code>sdd.lisp</code> files do not exist.
You get the same warning if lexer is not an executable. You can set them manually as follows:

a. set the <code>\*lexer\*</code> Lisp variable to your lexer binary. Make sure it follows the same i/o convention of
reading the characters from a file <code>fn</code> and writing the tokens to <code>fn.tokens</code> wrapped in (..).

b. set the <code>grammar, lexicon, lexforms</code> variables of <code>lalrparser.lisp</code> as you see fit.

c. do <code>(make-lalrparser)</code>. Now LALR tables are set. You are at step 2 above.



enjoy.
-cem bozsahin
