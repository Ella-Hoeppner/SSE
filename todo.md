* finish `sequentialize` for `Replace` and snippet operations

* The formatting logic sucks rn. Would be better to switch to something like how easl's formatter works, but more generalized.
  * one tricky consideration here is avoiding reformatting things in a way that would cause breaks in light of reserved tokens
    * e.g. if `&&` and `&` is a unary prefix operator, then `(& &a)` means something distinct from `(&&a)`. So the formatter would need to avoid turning any `& &` into `&&`, despite the fact that that would be perfectly valid for any other operator or if the `&&` keyword didn't exist
