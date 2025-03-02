```ebnf

<program> ::= { <declaration> }

<declaration> ::= <variable-declaration> | <function-declaration>
<variable-declaration> ::= { <specifier> }+ <declarator> [ "=" <initializer> ] ";"
<function-declaration> ::= { <specifier> }+ <declarator> ( <block> | ";" )

<specifier> ::= <type-specifier> | "static" | "extern"
<type-specifier> ::= "int" | "long" | "unsigned" | "signed" | "double" | "char" | "void"

<declarator> ::= "*" <declarator> | <direct-declarator>
<direct-declarator> ::= <simple-declarator> [ <declarator-suffix> ]
<simple-declarator> ::= <identifier> | "(" <declarator> ")"
<declarator-suffix> ::= <param-list> | { "[" <const> "]" }+
<param-list> ::= "(" "void" ")" | "(" <param> { "," <param> } ")"
<param> ::= { <type-specifier> }+ <declarator>

<initializer> ::= <exp> | "{" <initializer> { "," <initializer> } [ "," ] "}"

<block> ::= "{" { <block-item> } "}"
<block-item> ::= <statement> | <declaration>

<statement> ::= "return" [ <exp> ] ";"
              | <exp> ";"
              | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
              | <block>
              | "break" ";"
              | "continue" ";"
              | "while" "(" <exp> ")" <statement>
              | "do" <statement> "while" "(" <exp> ")" ";"
              | "for" "(" <for-init> [ <exp> ] ";" [ <exp> ] ")" <statement>
              | ";"
<for-init> ::= <variable-declaration> | [ <exp> ] ";"

<exp> ::= <cast-or-unary-exp> | <exp> <binop> <exp> | <exp> "?" <exp> ":" <exp>
<cast-or-unary-exp> ::= "(" <type-name> ")" <cast-or-unary-exp>
                      | <unary-exp>
<unary-exp> ::= <unop> <cast-or-unary-exp>
              | "sizeof" <unary-exp>
              | "sizeof" "(" <type-name> ")"
              | <postfix-exp>
<type-name> ::= { <type-specifier> }+ [ <abstract-declarator> ]
<postfix-exp> ::= <primary-exp> { "[" <exp> "]" }
<primary-exp> ::= <const>
                | <identifier>
                | "(" <exp> ")"
                | { <string> }+
                | <identifier> "(" [ <argument-list> ] ")"
<argument-list> ::= <exp> { "," <exp> }

<abstract-declarator> ::= "*" [ <abstract-declarator> ] | <direct-abstract-declarator>
<direct-abstract-declarator> ::= "(" <abstract-declarator> ")" { "[" <const> "]" }
                               | { "[" <const> "]" }+

<unop> ::= "-" | "~" | "!" | "*" | "&"
<binop> ::= "-" | "+" | "*" | "/" | "%" | "&&" | "||" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "="
<const> ::= <int> | <long> | <uint> | <ulong> | <double> | <char>
<identifier> ::= ? An identifier token ?
<string> ::= ? A string token ?
<int> ::= ? An int token ?
<char> ::= ? A char token ?
<long> ::= ? An int or long token ?
<uint> ::= ? An unsigned int token ?
<ulong> ::= ? An unsigned int or unsigned long token ?
<double> ::= ? A floating-point constant token ?

```
