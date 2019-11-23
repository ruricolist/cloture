# Cloture

Cloture is an implementation of Clojure in Common Lisp. It is designed to interoperate well with Common Lisp; e.g. Clojure is read by the Lisp reader and Clojure namespaces are Lisp packages.

Cloture is in very early (pre-alpha) stages, but it has progressed far enough to load a (lightly edited) version of test.clj, allowing the test suite to actually be written in Clojure.

Work so far has been focused on the critical path to get real Clojure code working in CL. But if there is interest from Clojurists I may work toward making it a more complete Clojure implementation.

## A note about FSet

Cloture uses [FSet][] seqs, maps, and sets to implement Clojure vectors, maps, and sets, respectively. This involves [a few hacks][fset-hacks] to FSet that might possibly affect other programs using FSet.

## Starting a REPL

Use `(cloture:repl)` to start a Clojure REPL. You can exit the REPL with `(quit)` or `(exit)`.

Note that not much work has been done yet on Clojure-style printing, so the “print” in REPL is still mostly the Common Lisp printer.

## Using Clojure from Lisp

The design goal of Cloture is to keep things as close to Common Lisp as possible: Clojure is read by the Lisp reader and Clojure namespaces are just packages. But of course Clojure is case sensitive, so you will need to use pipe characters to call, for example, `|clojure.core|:|cons|`.

Lisp’s nil is used only as the empty list; Clojure nil, true, and false are distinct objects. Use `cloture:truthy?` and `cloture:falsy?` with Clojure predicates.

Clojure files can be integrated into Lisp systems by making the system definition depend on Cloture `(:defsystem-depends-on ("cloture")` and using `"cloture:cljc"` as the file type.

    (defsystem ...
      :defsystem-depends-on ("cloture")
      :components ((:file "cloture:cljc" "my-clojure-code")))

## Using Lisp from Clojure

Since Clojure uses the Lisp reader, you can call Lisp functions just by uppercasing them.

    (letfn [(fst [xs] (CL:FIRST xs))]
      (fst '(1 2 3)))

You will also need to spell out `CL:QUOTE` and `CL:FUNCTION` (or refer them), as Clojure quote is not the same thing as CL quote and sharp-quote is used in Clojure for a different person.

    (ns ...
      (:require [CL :refer [QUOTE FUNCTION]]))

All Lisp sequences (lists, vectors, and extensible sequences on
implementations that support them) implement ISeq.

## Reader conditionals

In reader conditionals in `.cljc` files (and at the REPL), Cloture looks for a `:cl` key.

## License

[Eclipse Public License][EPL].

## Why “Cloture”?

Beside the obvious: [cloture][] is a parliamentary procedure to end debate on a subject, and I would like to end certain debates. Yes, Common Lisp is “modern.” Yes, Clojure is a Lisp.

[Clozure]: https://ccl.clozure.com/docs/ccl.html
[cloture]: https://en.wikipedia.org/wiki/Cloture
[EPL]: https://opensource.org/licenses/EPL-1.0
[ClojureScript]: https://clojurescript.org/
[FSet]: https://github.com/slburson/fset
[fset-hacks]: https://github.com/ruricolist/cloture/blob/master/fset-hacks.lisp
