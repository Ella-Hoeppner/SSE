# SSE

A parser for **S**ugared **S**-**E**xpressions.

### todo
* Maybe refactor `Sexp` to have a generic metadata param, to get rid of the need for `TaggedSexp`?
* Track all positioning while parsing, including whitespace
  * with this it should be possible to:
    * losslessly reconstruct the original tree from a `TaggedSexp`
    * identify, for a given line number and character position, where in the syntax tree it falls
* pretty printing
* have a flag to treat certain tags (or context tags?) as whitespace, from the outside
  * this is necessary for comments, to make sure they don't get included in the syntax tree
  * prove this works with a clj example with a comment
* support turning a `Sexp` back into a `TaggedSexp` for a given `SyntaxGraph`
* validate the coherence of syntax graph
  * things to validate:
    * no `ContextTag`s or `Tag`s should be duplicated
    * `Encloser`s are not symmetric
    * all `SyntaxTag`s have unique `tag_str`s
    * root `Tag` must exist in the syntax graph
    * all `Tag`s must exist in at least one context, or be the root
    * all `ContextTag`s should be reachable from root
    * no markers within a context are ambiguous, i.e. nothing is a prefix of another
      * or I guess, maybe allow markers that prefix others, but always give parsing precedence to the longer ones
* support for custom pretty-printing/formatting logic, conditional on tag + first element after tag
  * or maybe just accept a `fn(TaggedSexp) -> Option<Formatting Info>` that can scan each form and optionally give override control of the normal formatting, for more generality?
  * This will be useful for having clj-like autoformatting, where `let` and `fn` forms are formatted differently from normal applications, which is really nice
* maybe let enclosers consume args on the right/left too?
  * this would make bracket-generic syntax possible, e.g. `Type<Generic>`, where the `<...>` encloser consumes one left arg
