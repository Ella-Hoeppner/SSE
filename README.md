# SSE

A parser for **S**ugared **S**-**E**xpressions.

### todo
high priority:
* BUG: pure suffix ops always cause an error
* pretty printing
* generate info necessary for coloring text
* have a flag to treat certain context tags as comments
  * have a function for removing all of these from an AST
  * these shouldn't be included in the left/right consumption counts for operators

low priority:
* support for custom pretty-printing/formatting logic
  * this will be useful for things like putting names and bindings on the same line in `let`, or doing spacing differently than normal for certain special forms like `fn`
  * I guess this could work by giving the formatter a collection of (pattern, rule) pairs, where each pattern is a syntax tree (plus a context, maybe?) with one or more holes in it, and the rule describes how to format stuff inside of one of those holes. So like for `let` formatting, the pattern might look like `(let [<HOLE 1>] <HOLE 2>)`, and the rule might be like `in HOLE 1, place each group of 2 on the same line`
    * I guess this is kinda like part of a regex system. might want a general AST-regex system for other purposes like searching...
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
* maybe let enclosers consume args on the right/left too?
  * this would make bracket-generic syntax possible, e.g. `Type<Generic>`, where the `<...>` encloser consumes one left arg
