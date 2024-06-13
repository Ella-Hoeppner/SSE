# SSE

A parser for **S**ugared **S**-**E**xpressions.

### todo
* Bug: When an asymmetric closer is identical to a symmetric opener, it returns an `UnexpectedCloser` error.
  * E.g. the clj-like syntax has `"..."` for strings but also `#".."` for regexes, and trying to open a string fails because it sees the `"` as a closer of a `#"..."` rather than an opener of `"..."`
    * I guess I need an explicit check of openers before closers to fix this. But if I do that then is there any advantage to having `SymmetricEncloser` being a second thing? Maybe I can just get rid of that. Would simplify the API quite a bit.
* treating comments as sexps turns out to be a bit problematic - they get inserted as elements wherever they appear in the syntax tree, which is strange, and it also means they can take up space in operators, which is definitely a big problem
  * Probably need special logic to account for this. Maybe allow certain tags (or context tags?) to be treated, from the outside, as whitespace??
* create a clojure-like language as an example, including strings, single-line comments, and block-comments
* LSP
* support turning a `Sexp` back into a `TaggedSexp` for a given `SyntaxGraph`
* pretty printing
* Track all whitespace while parsing, such that the the original text can be losslessly recreated after parsing
  * I think this can just be in `TaggedSexp`, doesn't need to be in `Sexp`? Though sometimes it might be nice to have in `Sexp`?
    * Could just refactor things to have a single `Sexp` type that has generic parameters for metadata associated with each leaf, and each internal node. The current `Sexp` would just like having unit for the internal generic
  * this will be helpful in the LSP for highlighting errors and stuff
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
