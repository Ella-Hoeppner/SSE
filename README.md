# SSE

A parser for **S**ugared **S**-**E**xpressions.

### todo
* once a sexp is recognized, keep parsing until it is guaranteed that first sexp won't be consumed by an operator
* recognize mismatched closer markers of each encloser in scope, return error
  * including any in-scope closing encloser during infix ops
* support rebinding whitespace, escape sequences per tag
  * add tests
* support turning a `Sexp` back into a `TaggedSexp` for a given `SyntaxGraph`
* pretty printing
* create a context-free clojure-like language as an example, including strings, single-line comments, and block-comments

##### eventual todos, not urgent
* validate the coherence of syntax graph
  * things to validate:
    * no `ContextTag`s or `Tag`s should be duplicated
    * root `Tag` must exist in the syntax graph
    * all `Tag`s must exist in at least one context, or be the root
    * all `ContextTag`s should be reachable from root
    * no markers are ambiguous, i.e. nothing is a prefix of another
* maybe let enclosers consume args on the right/left too?
  * this would make bracket-generic syntax possible, e.g. `Type<Generic>`, where the `<...>` encloser consumes one left arg
* Track all whitespace while parsing, such that the the original text can be losslessly recreated after parsing
  * this will be helpful in the LSP for highlighting errors and stuff
* LSP
* support for custom pretty-printing/formatting logic, conditional on tag + first element after tag
  * or maybe just accept a `fn(TaggedSexp) -> Option<Formatting Info>` that can scan each form and optionally give override control of the normal formatting, for more generality?
* support lossless reading of whitespace?
