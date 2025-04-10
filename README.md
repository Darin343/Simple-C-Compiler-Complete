# Simple C Compiler
 
Compiler for Simple C, an abridged version of the C programming language. Simple C programs are valid C programs, but not all C features are supported (like function pointers, floating point arithmetic etc... ).

Targets 64 bit Intel x86 assembly running on Rocky Linux, written in C++.

Lexer.l constructs a C++ lexical analyzer using FLEX using regular expressions. Generates tokens for simple C keywords, operators, comments, identifiers, numbers, strings and chars from input sequences.

Parser.cpp implements a recursive descent parser. First, we disambiguated Simple C grammars so that only 1 parse tree may be constructed from any given input. Then, we established each level of precedence in its own function, starting with our global declarators. 

We then augment our parser to construct a symbol table. Simple C uses static nested scoping; we construct our symbol table to be a nested list of scopes. Each scope contains a vector of symbols, where each symbol contains information about an identifier (its type, level of indirection etc...). Our data structure is a basic linked list, where each scope contains a pointer to its enclosing scope. To perform semantic checks like matching names to identifiers, we can recursively search this list. We write checker.cpp to open and close scopes, and to report errors. Redefinition of functions, redeclaration of functions and variables, as well as undeclared name errors in input programs are now properly reported to std err.

While parsing, we also construct an abstract syntax tree as our intermediate representation (IR). This AST will allow us to generate correct assembly code for all simple C features. We use the Sethi-Ullman algorithm to generate optimal code on this tree; many optimizations however are left to be made. In the future, I may implement various improvements i.e. constant folding & propogation, loop hoisting, various operator strength reductions and so on. 
