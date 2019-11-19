# Cloture

Cloture is an implementation of Clojure in Common Lisp. It is designed to interoperate well with Common Lisp; e.g. Clojure is read by the Lisp reader and Clojure namespaces are Lisp packages.

Cloture is in very early (pre-alpha) stages, but it has progressed far enough to load a (lightly edited) version of test.clj, allowing the test suite to actually be written in Clojure.

Work so far has been focused on the critical path to get real Clojure code working in CL. But if there is interest from Clojurists I may work toward making it a more complete Clojure implementation.

## A note about FSet

Cloture uses [FSet][] seqs, maps, and sets to implement Clojure vectors, maps, and sets, respectively. This involves a few hacks to FSet that might possibly affect other programs using FSet.

## Using Clojure from Lisp

The design goal of Cloture is to keep things as close to Common Lisp as possible: Clojure is read by the Lisp reader and Clojure namespaces are just packages. But of course Clojure is case sensitive, so you will need to use pipe characters to call, for example, `|clojure.core|:|cons|`.

Lisp’s nil is used only as the empty list; Clojure nil, true, and false are distinct objects. Use `cloture:truthy?` and `cloture:falsy?` with Clojure predicates.

Clojure files can be integrated into Lisp systems by making the system definition depend on Cloture `(:defsystem-depends-on ("cloture")` and using `"cloture:cljc"` as the file type.

## Using Lisp from Clojure

Since Clojure uses the Lisp reader, you can call Lisp functions just by uppercasing them. However, this can get clumsy, so there is a reader macro, `#L`, that switches back to the Lisp reader. (Note that it does *not* change the package.)

## Reader conditionals

In reader conditionals in `.cljc` files, Cloture looks for a `:cl` key.

## License

[Eclipse Public License][EPL].

## Why “Cloture”?

Since I primarily use the [Clozure][] implementation of Common Lisp, and I am writing an implementation of Clojure, “[cloture][]” seemed like a name that would lead to no confusion whatsoever.

[Clozure]: https://ccl.clozure.com/docs/ccl.html
[cloture]: https://en.wikipedia.org/wiki/Cloture
[EPL]: https://opensource.org/licenses/EPL-1.0
[ClojureScript]: https://clojurescript.org/
[FSet]: https://github.com/slburson/fset
