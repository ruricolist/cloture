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
    (if (seq (rest forms))
      `(let [val# ~(first forms)]
         (if val# val#
             (or ~@(rest forms))))
      (first forms))
    nil))

(defn conj
  ([] [])
  ([coll] coll)
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

(defmacro when-let [[bind test] & body]
  `(if-let [~bind ~test] (do ~@body)))

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
         (let [k (first ks)
               ks (rest ks)]
           (recur (lookup m k not-found) ks))))))

(defn into
  ([] [])
  ([to] to)
  ([to from]
   (apply conj to (seq from)))
  ([to xform from]
   (transduce xform conj to from)))

(defn max
  ([x] x)
  ([x y] (if (< x y) y x))
  ([x y & more]
   (reduce max (max x y) more)))

(defn min
  ([x] x)
  ([x y] (if (< x y) x y))
  ([x y & more]
   (reduce min (min x y) more)))

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
  (reduce conj #{} xs))

(defn complement [f]
  (fn [& args] (not (apply f args))))

(defn mapcat [f & colls]
  (apply concat (apply map f colls)))

(defn some? [x]
  (not (nil? x)))

(defn any? [_] true)

(defn mapv [f & colls]
  (vec (apply map f colls)))

(defn filterv [pred coll]
  (vec (filter pred coll)))



(defn vary-meta [obj f & args]
  (with-meta obj (apply f (meta obj) args)))

(defn assoc
  ([map k v] (-assoc map k v))
  ([map k v & kvs]
   (let [m (-assoc map k v)]
     (if (seq kvs)
       (apply assoc m kvs)
       m))))

(defn update
  ([m k f] (assoc m k (f (get m k))))
  ([m k f & args] (assoc m k (apply f (get m k) args))))

(defn frequencies [coll]
  (reduce (fn [counts x] (assoc counts x (inc (get counts x 0)))) {} coll))

(defn some [pred coll]
  (let [s (seq coll)]
    (if s
      (let [v (pred (first s))]
        (if v v (some pred (rest s))))
      nil)))

(defn not-any? [pred coll] (not (some pred coll)))

(defn every? [pred coll]
  (let [s (seq coll)]
    (if s
      (if (pred (first s)) (every? pred (rest s)) false)
      true)))

(defn not-every? [pred coll] (not (every? pred coll)))

(defn take-while [pred coll]
  (lazy-seq
   (let [s (seq coll)]
     (when s
       (when (pred (first s))
         (cons (first s) (take-while pred (rest s))))))))

(defn drop-while [pred coll]
  (let [s (seq coll)]
    (if (and s (pred (first s)))
      (drop-while pred (rest s))
      s)))

(defn split-at [n coll] [(vec (take n coll)) (vec (drop n coll))])

(defn split-with [pred coll]
  [(vec (take-while pred coll)) (vec (drop-while pred coll))])

(defn not-empty [coll] (if (seq coll) coll nil))

(defn interleave
  ([] [])
  ([c1] (lazy-seq c1))
  ([c1 c2]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2)]
      (when (and s1 s2)
        (cons (first s1) (cons (first s2) (interleave (rest s1) (rest s2)))))))))

(defn map-indexed [f coll]
  (let [step (fn step [idx s]
               (lazy-seq
                (let [s (seq s)]
                  (when s
                    (cons (f idx (first s)) (step (inc idx) (rest s)))))))]
    (step 0 coll)))

(defn keep-indexed [f coll]
  (filter some? (map-indexed f coll)))

(defn partition
  ([n coll] (partition n n coll))
  ([n step coll]
   (lazy-seq
    (let [s (seq coll)]
      (when s
        (let [p (vec (take n s))]
          (when (= n (count p))
            (cons p (partition n step (drop step s))))))))))

(defn partition-all
  ([n coll] (partition-all n n coll))
  ([n step coll]
   (lazy-seq
    (let [s (seq coll)]
      (when s
        (cons (vec (take n s)) (partition-all n step (drop step s))))))))

(defn run! [proc coll]
  (reduce (fn [_ x] (proc x) nil) nil coll)
  nil)

(defn distinct? [& xs] (= (count xs) (count (set xs))))

(defn requiring-resolve [sym]
  (let [v (resolve sym)]
    (if v
      v
      (do (require (symbol (namespace sym)))
          (resolve sym)))))

(defn some-fn [& preds]
  (fn [& args]
    (some (fn [p] (some p args)) preds)))

(defn every-pred [& preds]
  (fn [& args]
    (every? (fn [p] (every? p args)) preds)))

(defn coll? [x]
  (or (vector? x) (map? x) (set? x) (seq? x)))

(defn list? [x]
  (and (seq? x) (not (vector? x)) (not (map? x)) (not (set? x))))
