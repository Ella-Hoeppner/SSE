# GSE

A parser for **S**ugared **S**-**E**xpressions.

### todo
* handle operators consuming leftwards arguments
  * error when there aren't enough
* recognize mismatched closer markers of each encloser in scope, return error
  * including any in-scope closing encloser
* write a lot of tests
  * arithmetic precedence
  * contextful enclosers/operators: 
    * Have a special syntax in which `<` and `>` are used as enclosers at the outermost level, but inside of `(` and `)` enclosers, let `<` and `>` be normal symbols, such that they could be used as math operators
    * Have `:` have different syntax on each side
* support rebinding whitespace, escape sequences per tag
  * tests
* support turning a `Sexp` back into a `TaggedSexp` for a given `SyntaxGraph`
* pretty printing
* create a context-free clojure-like language as an example, including strings, single-line comments, and block-comments

##### eventual todos, not urgent
* LSP
* support for custom pretty-printing/formatting logic, conditional on tag + first element after tag
  * or maybe just accept a `fn(TaggedSexp) -> Option<Formatting Info>` that can scan each form and optionally give override control of the normal formatting, for more generality?
* support lossless reading of whitespace?
