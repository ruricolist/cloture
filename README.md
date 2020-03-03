# Cloture

Cloture is an implementation of Clojure in Common Lisp. It is designed above all to interoperate well with Common Lisp; Clojure is read by the Lisp reader and Clojure namespaces are Lisp packages.

Cloture is in very early (pre-alpha) stages, but it has progressed far enough to load [clojure.test](test.clj), allowing the [test suite][] to actually be written in Clojure.

Work so far has been focused on the critical path to get real Clojure code working in CL. But if there is interest from Clojurists I may work toward making it a more complete Clojure implementation.

## Clojure vs. ClojureScript

Cloture is closer to Clojure than to ClojureScript. Among other things, the plan is to support Clojure’s concurrency primitives (`atom`, `ref`, `agent`, `future`, `promise`). However, Cloture follows ClojureScript in making exclusive use of protocols - interfaces are not used or supported. Protocol names are also derived from ClojureScript.

Like ClojureScript, Cloture supports `(catch :default)` to catch everything.

## A note about FSet

Cloture uses [FSet][] seqs, maps, and sets to implement Clojure vectors, maps, and sets, respectively. This involves [a few hacks][fset-hacks] to FSet that might possibly affect other programs using FSet.

## Starting a REPL

Use `(cloture:repl)` to start a Clojure REPL. You can exit the REPL with `(quit)` or `(exit)`.

Note that not much work has been done yet on Clojure-style printing, so the “Print” in REPL is still mostly the Common Lisp printer.

## Interoperation

### Using Clojure from Lisp

The design goal of Cloture is to keep things as close to Common Lisp as possible: Clojure is read by the Lisp reader and Clojure namespaces are just packages. Clojure packages and functions, however, usually have lower-case names, so to call them from Lisp you will need to quote them with pipe characters:

    (|clojure.core|:|cons| 1 '(2))
    => '(1 2)

Lisp’s nil is used only as the empty list; Clojure nil, true, and false are singletons. To use Clojure predicates from Lisp, you can use `cloture:truthy?` to translate.

    (cloture:truthy? (|clojure.core|:|=| '(1 2 3) #(1 2 3)))
    => T

In this case, however, you should use `cloture:egal`, which tells you if two objects are equal according to Clojure’s idea of equality.

Cloture exports Iterate drivers for working with collections that satisfy Clojure protocols:

- `cloture:in-seq` iterates over `ISeq`.
- `cloture:on-seq` iterates over the rests of `ISeq`.
- `cloture:in-indexed` iterates over `IIndexed`.
- `cloture:index-of-indexed` iterates over the indices of `IIndexed`.

Clojure files can be integrated into Lisp systems by making the system definition depend on Cloture `(:defsystem-depends-on ("cloture")` and using `"cloture:cljc"` as the file type.

    (defsystem ...
      :defsystem-depends-on ("cloture")
      :components ((:file "cloture:cljc" "my-clojure-code")))

You can also use `"cloture:clj"` or `"cloture:cljs"` to load straight Clojure or ClojureScript files. Using `.cljc` is recommended, however.

### Using Lisp from Clojure

Since Clojure uses the Lisp reader, you can call Lisp functions just by uppercasing them.

    (letfn [(fst [xs] (CL:FIRST xs))]
      (fst '(1 2 3)))

You will also need to spell out `CL:QUOTE` and `CL:FUNCTION` (or refer them), as Clojure quote is not the same thing as CL quote and sharp-quote is used in Clojure for a different purpose.

    (ns ...
      (:require [CL :refer [QUOTE FUNCTION]]))

Cloture defines a Clojure namespace, `cloture`, with exports whose names (and keyword arguments!) are already conveniently lowercased and otherwise follow Clojure conventions:

    (ns ...
      (:require [cloture:refer [parse-integer]]))

    (parse-integer "1234x" :start 1 :junk-allowed true)
    => 234

All Lisp sequences (lists, vectors, and extensible sequences on implementations that support them) implement ISeq.

## Reader conditionals

In reader conditionals in `.cljc` files (and at the REPL), Cloture looks for a `:cl` key.

## License

[Eclipse Public License][EPL].

## Why?

I would like to be able to use Clojure libraries from Common Lisp.

## Why “Cloture”?

Beside the obvious: [cloture][] is a parliamentary procedure to end debate on a subject, and I would like to end certain debates. Yes, Common Lisp is “modern.” Yes, Clojure is a Lisp.

[Clozure]: https://ccl.clozure.com/docs/ccl.html
[cloture]: https://en.wikipedia.org/wiki/Cloture
[EPL]: https://opensource.org/licenses/EPL-1.0
[ClojureScript]: https://clojurescript.org/
[FSet]: https://github.com/slburson/fset
[fset-hacks]: https://github.com/ruricolist/cloture/blob/master/fset-hacks.lisp
[test.clj]: https://github.com/ruricolist/cloture/blob/master/clojure/test.cljc
[test suite]: https://github.com/ruricolist/cloture/blob/master/t/cloture-tests.cljc
