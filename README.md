# Cloture

Cloture is a (partial, pre-alpha) implementation of Clojure in Common Lisp. I am releasing now because it has progressed far enough for its test suite to actually be written in Clojure.

My current goal with Cloture is to reduce the work of porting Clojure libraries to Common Lisp to the point where ports can just be forks, rather than complete rewrites, and future improvements can be merged from upstream. But if there is interest from Clojurists I may work on making it a more complete Clojure implementation.

## Using Clojure from Lisp

The overriding design goal of Cloture is to keep things as close to Common Lisp as possible: Clojure is read by the Lisp reader and Clojure namespaces are just packages. But of course Clojure is case sensitive, so you will need to use pipe characters to call, for example, `|clojure.core|:|cons|`.

Lisp’s nil is used only as the empty list; Clojure nil, true, and false are distinct objects. Use `cloture:truthy?` and `cloture:falsy?` with Clojure predicates.

Clojure files can be integrated into Lisp systems by making the system definition depend on Cloture `(:defsystem-depends-on ("cloture")` and using `"cloture:cljc"` as the file type.

## Using Lisp from Clojure

Clojure uses the Lisp reader, so all you have to do to use Lisp from Clojure is to, like Death itself, SPEAK IN UPPERCASE. (Obviously Lisp reader macros may be overwritten by Clojure.) This also applies when defining a namespace; you can refer or require CL packages, you just have to SHOUT.

## Protocols

Cloture is similar to [ClojureScript][] in that it makes exclusive use of protocols; interfaces are not used. But Cloture protocols use the same name as the functions they extend; they do not have prefixes as in ClojureScript. (What’s that about, anyway)?

Lispers: relax, they’re just generic functions.

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
