;;; -*- mode: clojure -*-
;;; This file is the bootstrapping inflection point -- it contains
;;; implementations of clojure.core functions written in Clojure.

(in-package "clojure.core")
(named-readtables:in-readtable cloture:cloture)

(defn get-in
  ([m ks] (get-in m ks nil))
  ([m ks not-found]
   (loop [m m ks ks]
     (if (not (seq ks)) m
         (let [ks (first ks)
               ks (rest ks)]
           (recur (lookup m ks not-found) ks))))))

(defn into
  ([] [])
  ([to] to)
  ([to from]
   (apply conj to (seq from))))
