# SSE

A parser for **S**ugared **S**-**E**xpressions.

### todo
* diffs:
  * diff reversal, for undo/redo functionality

* don't include comments in left/right consumption counts for operators

low priority:
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
