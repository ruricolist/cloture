;;; -*- mode: clojure -*-

(in-package "clojure.core")
(named-readtables:in-readtable cloture:cloture)

(defn empty? [xs]
  (not (seq xs)))
