# Cloture

Cloture is a library to let you load Clojure files as if they were Common Lisp. It is not a complete Clojure implementation; it just  automates the parts that can be easily automated. The idea is to allow Common Lisp ports of Clojure libraries to be forks, rather than complete rewrites, so future improvements can be merged from upstream.

Maybe once this would have seemed impossible, but if Clojure can be implemented in JavaScript it can certainly be implemented in Common Lisp.

## License

While Cloture contains no code from Clojure, as a gesture of respect it is released under the [Eclipse Public License][EPL].

## Why “Cloture”?

Since I primarily use the [Clozure][] implementation of Common Lisp, and I am writing a bridge to Clojure, [cloture][] seemed like a name that would lead to no confusion whatsoever.

[Clozure]: https://ccl.clozure.com/docs/ccl.html
[cloture]: https://en.wikipedia.org/wiki/Cloture
[EPL]: https://opensource.org/licenses/EPL-1.0
