# HaskellMiniLispREPL
A REPL for "mini-Lisp", a very simple functional PL operating on integers.

Requires parsec for string parsing (https://hackage.haskell.org/package/parsec)

Syntax:
<expr> = <int> (a literal int)  
        |<id>  (a variable id, must be only letters)  
        |(set <id> <expr>)  (assigns the value of <expr> to variable <id>, <id> must only be letters)  
        |(+ <expr> <expr>)  (addition)  
        |(- <expr1> <expr2>)  (subtracts <expr2>'s value from <expr1>'s value)  
        |(* <expr> <expr>)  (multiplication)
