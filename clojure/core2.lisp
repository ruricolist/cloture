;;; -*- mode: clojure -*-
;;; This file is the bootstrapping inflection point -- it contains
;;; implementations of clojure.core functions written in Clojure.

(in-package "clojure.core")
(named-readtables:in-readtable cloture:cloture)

(defn nil? [x]
  (identical? x nil))

(defn true? [x]
  (identical? x true))

(defn false? [x]
  (identical? x false))

(defn identity [x] x)

(defmacro when [test & body]
  `(if ~test (do ~@body)))

(defmacro when-not [test & body]
  `(when (not ~test) ~@body))

(defmacro if-not
  ([test then] `(if (not ~test) ~then))
  ([test then else] `(if (not ~test) ~then ~else)))

(defmacro and [& forms]
  (if (seq forms)
    `(let [val# ~(first forms)]
       (if-not val# val#
               (and ~@(rest forms))))
    true))

(defmacro or [& forms]
  (if (seq forms)
    `(let [val# ~(first forms)]
       (if val# val#
           (or ~@(rest forms))))
    nil))

(defn conj
  ([coll x] (-conj coll x))
  ([coll x & xs] (reduce -conj coll (cons x xs))))

(defn dissoc
  ([coll k] (-dissoc coll k []))
  ([coll k & ks] (-dissoc coll k ks)))

(defn not= [& xs]
  (not (apply = xs)))

(defn get
  ([map k] (get map k nil))
  ([map k not-found] (lookup map k not-found)))

(defn empty? [xs]
  (not (seq xs)))

(defmacro if-let
  ([binds then] `(if-let ~binds ~then nil))
  ([[binds test] then else]
   `(let [temp# ~test]
      (if temp#
        (let [~binds temp#]
          ~then)
        ~else))))

(defmacro when-let
  ([binds & body]
   `(if-let ~binds ~@body)))

(defn fnil
  ([f x]
   (fn [arg1 & args]
     (apply f
            (if (nil? arg1) x arg1)
            args)))
  ([f x y]
   (fn [arg1 arg2 & args]
     (apply f
            (if (nil? arg1) x arg1)
            (if (nil? arg2) y arg2)
            args)))
  ([f x y z]
   (fn [arg1 arg2 arg3 & args]
     (apply f
            (if (nil? arg1) x arg1)
            (if (nil? arg2) y arg2)
            (if (nil? arg3) z arg3)
            args))))

(defn nthnext [coll n]
  (if (zero? n)
    (seq coll)
    (recur (next coll) (dec n))))

(defn nthrest [coll n]
  (if (zero? n)
    (seq coll)
    (recur (rest coll) (dec n))))

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

(defn max
  ([x] x)
  ([x y] (if (< x y) y x))
  ([x y & more]
   (reduce max (max x y) more)))

(defn hash-ordered-coll [collection]
  (-> (reduce (fn [acc e] (unchecked-add-int
                           (unchecked-multiply-int 31 acc)
                           (hash e)))
              1
              collection)
      (mix-collection-hash (count collection))))

(defn hash-unordered-coll [collection]
  (-> (reduce unchecked-add-int 0 (map hash collection))
      (mix-collection-hash (count collection))))

(defn repeat
  ([x] (repeatedly (constantly x)))
  ([n x] (repeatedly n (constantly x))))

(defn interpose [sep coll]
  (if (not (seq coll)) '()
      (if (not (seq (rest coll))) coll
          (concat (list (first coll) sep)
                  (interpose sep (next coll))))))

(defn second [x] (first (next x)))
(defn fnext [x] (first (next x)))
(defn ffirst [x] (first (first x)))
(defn nfirst [x] (next (first x)))
(defn nnext [x] (next (next x)))

(defn merge [& maps]
  (apply merge-with second maps))

(defn assoc-in [m ks v]
  (update-in m ks (constantly v)))

(defn take [n xs]
  (if (and (pos? n)
           (seq xs))
    (lazy-seq (cons (first xs)
                    (take (dec n) (rest xs))))
    '()))

(defn set [xs]
  (reduce conj (empty-set) xs))
