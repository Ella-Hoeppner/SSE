# GSE

A parser for **G**eneralized **S**-**E**xpressions.

## Background and Motivation

S-expressions are a simple but powerful text format that come from the Lisp family of programming languages. Each S-expression is either an *atom* or a *list*, where an atom is a simple indivisible token like a word, number, or string, and a list is a sequence of simpler S-expressions enclosed in parentheses and separated by whitespace. The following are all valid S-expressions:

```
hello

(hello)

(hello goodbye)

(hello goodbye (hello again))

(s-expressions ((can be) (arbitrarily (deeply (nested!)))))
```

Most Lisps add a few extra syntactic conveniences on top of pure S-expressions. A common example is the quote operator, usually denoted with the `'` character, which can be used as a prefix to an S-expression to denote that the following S-expression should be treated as data, rather than code to be executed. For instance, `'hello` and `'(hello goodbye)` are valid expressions in most Lisps. Prefix operators like this can be interpreted as simple syntactic sugar over normal S-expressions, e.g. the previous examples could be seen as shorthand for `(quote hello)` and `(quote (hello goodbye))`, respectively.

Clojure, a modern Lisp dialect, diverges even further from a pure S-expression syntax by using several distinct types of delimiters aside from parentheses, with each kind of delimiter corresponding to a different data structure or semantic element. While `(...)` is used to represent lists, `[...]` is used to represent vectors, `{...}` for hashmaps, `#{...}` for sets, and `#(...)` for lambdas. These extra delimiters allow Clojure code to be more expressive and concise than traditional Lisps, while staying close enough to traditional S-expressions to maintain the benefits like [structural editing](https://clojure.org/guides/structural_editing).

This project aims to establish a new text format, called "GSE" for **G**eneralized **S**-**E**xpressions, that extends traditional S-expressions with the capacity to handle arbitrary (unary) prefix operators, as well as arbitrary delimiters. Just as prefixes like `'` are commonly treated as syntactic sugar that can be expanded to a pure S-expression, GSE treats different prefixes and delimiters as a syntactic sugar that appends an identitifying tag the beginning of a normal S-expression. The GSE parser doesn't make any assumptions about what specific prefix operators or delimiters are allowed, and instead can be told to treat any sequence of characters as a prefix operator or delimiter.

The goal in defining this new format is to provide a unified syntactic framework that can be shared between all S-expression based languages. As such, GSE can provide a foundation for shared tooling between different languages that abstracts away minor details like what kind of prefix operators or delimiters are available in a language. GSE will be able to encapsulate the syntax of most existing Lisps, including Clojure, Scheme, and Common Lisp, but the main purpose of GSE is to provide a convenient shared syntactic framework for future languages. Specfically, a core goal of GSE is to make it easy to define alternative syntaxes for non-lispy languages that can be easily transpiled to and from the original syntax, allowing these languages to gain the benefits of Lisp syntax, including things like macros, structural editing, and, potentially, [visual editing](https://github.com/Ella-Hoeppner/Vlojure).

## to do
* Make delimiters and prefixes context-sensitive, i.e. what delimiters/prefixes are available depends on what delimiter/prefix the parser inside of
* Support escape characters inside delimiters/prefix
  * This should be determined by a function in the `Delimiter` and `Prefix` trait
* Handle strings as a special case of delimiter
  * will just be a symmetric delimiter with `"` as the opener and closer, and `\` as an escape character
* Parser should operate over a &str or at least a String, rather than a Vec<char>
* `GSexp::from_sexp`
  * Converts a pure-sexp back into `GSexp`, given a set of prefix/delimiter types
    * will have to figure out how to handle cases where there are multiple delimiters/prefixes with the same tag...
      * acutally maybe just don't allow duplicate tags?
      * could just always use the first one, but then converting back to GSE would be lossy
      * I guess when there are multiple options for which delimiter/prefix to use, each element could store an index for which one it came from
* keep track of character indeces for each element, such that the original string can be recreated losslessly
  * I guess maybe we also need a "trailing whitespace" field or something, to distinguish spaces from tabs from newlines...
  * should make a bunch of tests for this
* Once the above is all done and there are extensive tests:
  * Represent clojure/script syntax in GSE
  * Integrate GSE into Cast
  * work on an LSP for syntax highlighting (specifically rainbow-colored delimiters) and paredit-like structural editing
    * or at least, part of an LSP. Whatever I build here should be easily extensible to include things you'd want if you built a full language on top of GSE, like definition lookups and error squiggles.