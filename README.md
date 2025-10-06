# SSE

A parser for **S**ugared **S**-**E**xpressions.

### todo
* don't include comments in left/right consumption counts for operators

* gracefully handle missing inputs to operators
  * Should still return a mostly-valid tree, but just with the specific operator usages marked as invalid, I think

* The formatting logic sucks rn. Would be better to switch to something like how easl's formatter works, but more generalized.
  * one tricky consideration here is avoiding reformatting things in a way that would cause breaks in light of reserved tokens
    * e.g. if `&&` and `&` is a unary prefix operator, then `(& &a)` means something distinct from `(&&a)`. So the formatter would need to avoid turning any `& &` into `&&`, despite the fact that that would be perfectly valid for any other operator or if the `&&` keyword didn't exist

low priority:
* maybe let enclosers consume args on the right/left too?
  * this would make bracket-generic syntax possible, e.g. `Type<Generic>`, where the `<...>` encloser consumes one left arg
