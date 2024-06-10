# SSE

A parser for **S**ugared **S**-**E**xpressions.

### todo
* rather than specifying a unique set of children for each syntax element, have a notion of a "scope" that specifies the allowed children, and simply let each syntax element point to it's "scope"
  * I guess there will need to be a "ScopeTag" type as well... the generics are probably gunna get complicated :O
  * This will make things easier when it comes to having custom whitespace/escape definitions for each scope
* once a sexp is recognized, keep parsing until it is guaranteed that first sexp won't be consumed by an operator
* recognize mismatched closer markers of each encloser in scope, return error
  * including any in-scope closing encloser during infix ops
* validate the coherence of syntax graph
  * I guess there'll need to be a `SyntaxGraphBuilder`? Or just require `SyntaxGraph` to take all of its syntax elements as arguments to `new` rather than having the `with_` methods
  * things to validate:
    * all tags should be reachable from root
    * no markers are ambiguous, i.e. nothing is a prefix of another
* support rebinding whitespace, escape sequences per tag
  * add tests
* support turning a `Sexp` back into a `TaggedSexp` for a given `SyntaxGraph`
* pretty printing
* create a context-free clojure-like language as an example, including strings, single-line comments, and block-comments

##### eventual todos, not urgent
* maybe let enclosers consume args on the right/left too?
  * this would make bracket-generic syntax possible, e.g. `Type<Generic>`, where the `<...>` encloser consumes one left arg
* Track all whitespace while parsing, such that the the original text can be losslessly recreated after parsing
  * this will be helpful in the LSP for highlighting errors and stuff
* LSP
* support for custom pretty-printing/formatting logic, conditional on tag + first element after tag
  * or maybe just accept a `fn(TaggedSexp) -> Option<Formatting Info>` that can scan each form and optionally give override control of the normal formatting, for more generality?
* support lossless reading of whitespace?
