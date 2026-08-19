(ns cloture.tests
  (:require [clojure.test :refer [deftest is are testing]])
  (:require [clojure.string :as s])
  (:require [clojure.walk :as walk])
  (:require [cloture :refer [parse-integer]]))

(deftest empty-test)

(deftest trivial-test
  (is true))

;;; Examples from the clojure.test documentation.

(deftest addition
  (is (= 4 (+ 2 2)))
  (is (= 7 (+ 3 4))))

(deftest subtraction
  (is (= 1 (- 4 3)))
  (is (= 3 (- 7 4))))

(deftest arithmetic
  (addition)
  (subtraction))

(deftest sanity-check
  (is true)
  (is (not false))
  (is (not nil))
  (is '())
  (is (not= nil '()))
  (is (= 0 0))
  (is (not= 0 1))
  (is (not= 1 0))
  (is (= 1 1))
  (is (not= 1 -1))
  (is (not= nil false))
  ;; The empty list is a truthy value.
  (is CL:NIL))

(deftest equality-sanity-check
  (is (= 0 0))
  (is (= (hash 0) (hash 0)))
  (is (not= 0 1))
  (is (not= (hash 0) (hash 1))))

(deftest read-vector
  (is (= '(1 2 3 4) [1 2 3 4]))
  (is (not= '(1 2 3) [1 2 3 4]))
  (is (not= '(1 2 3 4) [1 2 3]))
  (is (not= '(2 3 4) [1 2 3 4]))
  (is (not= '(1 2 3 4) [2 3 4])))

(deftest read-map
  (is (= '([:x 1] [:y 2] [:z 3])
         (seq {:x 1 :y 2 :z 3})))
  ;; Clojure's behavior differs from FSet here; worth fixing?
  ;; (is (not= '([:x 1] [:y 2] [:z 3])
  ;;           (seq {:y 2 :x 1  :z 3})))
  (is (not= '([:x 1] [:y 2])
            (seq {:x 1 :y 2 :z 3}))))

(deftest read-set
  (is (set? #{1 2 3}))
  (is (not (map? #{1 2 3})))
  (is (not (set? {1 2})))
  (is (map? {1 2})))

(deftest read-meta
  (let [sym (read-string "^:dynamic *bar*")]
    (is (symbol? sym))
    (is (get (meta sym) :|dynamic|))))

(deftest test-read-char
  (is (= \\ (first "\\")))
  (is (= "\\" (str \\)))
  (is (= \{ (first "{")))
  (is (= \{ '\{))
  (is (= \newline (first "\n")))
  (is (= \tab (first "\t")))
  (is (= \u03A9 (first "Ω"))))

(deftest test-string-escapes
  (is (= "\u03A9" "Ω")))

(deftest test-read-hex
  (is (= 0xfff 4095)))

(deftest test-read-octal
  (is (= 0123 83)))

(deftest let-test
  (is (= 3 (let [x 1 y 2] (+ x y))))
  (is (= 3 (let [x 2 y 1] (+ x y))))
  (is (not= 3 (let [x 1 y 1] (+ x y)))))

(deftest commas
  (is (= '() (,,,,,)))
  (is (= '(:x :y :z) '(:x, :y, :z))))

(deftest qq
  (is (= '(:x) `(~:x)))
  (is (not= '(:x) `(~:X))))

(deftest reader-conditional
  (is (= 1 #?(:cl 1 :clj 2))))

(deftest reader-conditional-splicing
  (is (= [1 2 3]
         (vector #?@(:cl [1 2 3])))))

(deftest reader-conditional-splicing-clr
  ;; See https://clojure.org/guides/reader_conditionals.
  (testing "splicing"
    (is (= [] [#?@(:cl [])]))
    (is (= [:a] [#?@(:cl [:a])]))
    (is (= [:a :b] [#?@(:cl [:a :b])]))
    (is (= [:a :b :c] [#?@(:cl [:a :b :c])]))
    (is (= [:a :b :c] [#?@(:cl [:a :b :c])]))))

#?@(:cl
    [(defn clj-fn1 [] :abc)
     (defn clj-fn2 [] :cde)])

(deftest reader-conditional-splicing-toplevel
  (is (= (clj-fn1) :abc))
  (is (= (clj-fn2) :cde)))

(deftest destructure-simple
  (is (= '(1 2 3)
         (let [[x y z] [1 2 3]]
           (list x y z))))
  (is (not= '(1 2 3 4)
            (let [[x y z] [1 2 3]]
              (list x y z))))
  (is (not= '(1 2)
            (let [[x y z] [1 2 3]]
              (list x y z))))
  (is (= '(1 2 3)
         (let [[x y z] '(1 2 3)]
           (list x y z)))))

(deftest destructure-as
  (is (= '(1 2 3)
         (let [[_ _ _ :as all] [1 2 3]]
           all)))
  (is (= '(1 2 3)
         (let [[_ _ _ :as all] '(1 2 3)]
           all))))

(deftest destructure-rest
  (is (= '(2 3)
         (let [[_ & ys] [1 2 3]]
           ys)))
  (is (= '(2 3)
         (let [[_ & ys] '(1 2 3)]
           ys))))

(deftest destructure-rest-and-as
  (is (= '(1 2 3)
         (let [[_ & _ :as all] [1 2 3]]
           all)))
  (is (= '(1 2 3)
         (let [[_ & _ :as all] '(1 2 3)]
           all))))

(deftest destructure-nested
  (is (= '(1 2 3 4 5 6)
         (let [[[a] [[b]] c [x y z]] [[1] [[2]] 3 [4 5 6]]]
           (list a b c x y z)))))

(deftest destructure-short
  (let [l1 (list nil)
        l2 (let [[x] '()]
             (list x))]
    (is (= l1 l2))))

(deftest destructure-lisp-vector
  (is (= #=(CL:VECTOR 1 2 3 4 5)
         (let [[_ _ _ _ _ :as all] #=(CL:VECTOR 1 2 3 4 5)]
           all)))
  (is (= '(1 2 3 4 5)
         (let [[a b c d e] #=(CL:VECTOR 1 2 3 4 5)]
           (list a b c d e)))))

(def client
  {:name "Super Co."
   :location "Philadelphia"
   :description "The worldwide leader in plastic tableware."})

(deftest destructure-associative
  (is (= "Super Co. Philadelphia - The worldwide leader in plastic tableware."
         (let [{name :name
                location :location
                description :description}
               client]
           (print-str name location "-" description)))))

(deftest destructure-associative-missing
  (is (nil? (let [{category :category} client] category))))

(deftest destructure-associative-as
  (is (= client (let [{:as all} client] all))))

(deftest destructure-keyword-arguments
  (letfn [(optify [& {:as opts}] opts)]
    (is (= {:x 1 :y 2}
           (optify :x 1 :y 2)))))

(deftest destructure-associative-default
  (is (= "Category not found"
         (let [{category :category, :or {category "Category not found"}} client]
           category))))

(deftest destructure-keys
  (is (= "Super Co. Philadelphia - The worldwide leader in plastic tableware."
         (let [{:keys [name location description]} client]
           (print-str name location "-" description)))))

(def string-keys {"first-name" "John" "last-name" "Doe"})

(deftest destructure-strs
  (is (= "John Doe"
         (let [{:strs [first-name last-name]} string-keys]
           (print-str first-name last-name)))))

(def symbol-keys {'first-name "Jane" 'last-name "Doe"})

(deftest destructure-syms
  (is (= "Jane Doe"
         (let [{:syms [first-name last-name]} symbol-keys]
           (print-str first-name last-name)))))

(def multiplayer-game-state
  {:joe {:class "Ranger"
         :weapon "Longbow"
         :score 100}
   :jane {:class "Knight"
          :weapon "Greatsword"
          :score 140}
   :ryan {:class "Wizard"
          :weapon "Mystic Staff"
          :score 150}})

(deftest destructure-associative-nested
  (is (= "Joe is a Ranger wielding a Longbow"
         (let [{{:keys [class weapon]} :joe} multiplayer-game-state]
           (print-str "Joe is a" class "wielding a" weapon)))))

(deftest fn-test
  (let [bar
        (fn bar
          ([a b]
           (bar a b 100))
          ([a b c]
           (* a b c)))]
    (is (= 3000 (bar 5 6)))
    (is (= 60 (bar 5 6 2)))))

(deftest letfn-test-1
  (letfn [(bar
            ([a b]
             (bar a b 100))
            ([a b c]
             (* a b c)))]
    (is (= 3000 (bar 5 6)))
    (is (= 60 (bar 5 6 2)))))

(deftest letfn-test-2
  (is (= 1
         (letfn [(fst [xs] (CL:FIRST xs))]
           (fst '(1 2 3)))))
  ;; Waiting on a code walker.
  ;; (is (= 1
  ;;        ((letfn [(fst [xs] (CL:FIRST xs))]
  ;;           fst)
  ;;         '(1 2 3))))
  )

(deftest test-thread-first
  (is (seq (-> '((1 2) (3 4)))))
  (is (= 2 (-> '((1 2) (3 4)) first second)))
  (is (= 3 (-> '((1 2) (3 4)) second first))))

(deftest test-thread-last
  (= 3/4 (->> 5 (+ 3) (/ 2) (- 1))))

(deftest loop-recur
  (let [fact
        (fn [n]
          (loop [cnt n
                 acc 1]
            (if (zero? cnt)
              acc
              (recur (dec cnt) (* acc cnt)))))]
    (= (fact 10)
       (ALEXANDRIA:FACTORIAL 10))))

(def ^{:dynamic true :private true} foo* 0)
(defn ^:private get-foo [] foo*)

(deftest binding-test
  (is (= 0 foo*))
  (is (= 1 (let [foo* 1] foo*)))
  (is (= 0 (let [foo* 1] foo* (get-foo))))
  (is (= 1 (binding [foo* 1] foo*)))
  (is (= 1 (binding [foo* 1] (get-foo)))))

(deftest var-test
  (is (= '(var foo*) '#'foo*)))

(deftest seq-test
  (is (nil? (seq '())))
  (is (nil? (seq {})))
  (is (nil? (seq #{})))
  (is (nil? (seq [])))
  (is (ALEXANDRIA:SET-EQUAL '(1 2 3) (seq #{1 2 3})))
  (is (not= '([:x 1] [:y 2])
            (seq {:X 1 :Y 2})))
  (is (= '([:x 1] [:y 2])
         (seq {:x 1 :y 2}))))

(deftest test-empty?
  (is (empty? '()))
  (is (empty? nil)))

(deftest read-nothing
  (is (= '(1 2) '(1 2 #_3)))
  (is (= '(1) '(1 #_#_2 3))))

(deftest read-conditional
  (is (= [] [#?(:foo true)])))

(def ^:private hello (fn hello [] "hello"))
(def ^{:private true :dynamic true} *hello* (fn hello [] "hello"))

(deftest lisp-1
  (is (= "hello" (hello)))
  (is (= "goodbye"
         (let [hello (constantly "goodbye")]
           (hello))))
  (is (= "goodbye"
         (let [[hello] (list (constantly "goodbye"))]
           (hello)))))

(deftest pop-test
  (is (= [1 2] (pop [1 2 3])))
  (is (= [1] (pop [1 2])))
  (is (= [] (pop [1])))
  (is (thrown? IllegalStateException (pop []))))

(deftest unqualified-symbol-equality
  (is (not (identical? 'clojure.test::thrown? 'thrown?)))
  (is (= 'clojure.test::thrown? 'thrown?))
  (is (= 1 (get {'clojure.test::thrown? 1}
                'thrown?)))
  (is (= (hash 'clojure.test::thrown?)
         (hash 'thrown?))))

(deftest re-find-test
  (is (nil? (re-find #"sss" "Loch Ness")))
  (is (= "ss" (re-find #"s+" "dress")))
  (is (= ["success" "ucces" "s"] (re-find #"s+(.*)(s+)" "success"))))

(deftest re-matches-test
  (is (nil? (re-matches #"abc" "zzzabcxxx")))
  (is (= "abc" (re-matches #"abc" "abc")))
  (is (= ["abcxyz" "xyz"] (re-matches #"abc(.*)" "abcxyz"))))

(deftest qq-seq-ok
  (let [body '(x)]
    (is (= `(let [x 1]
              ~@body)
           '(let [x 1] x)))))

(deftest qq-seq-1
  (is (= '[:x 1] `[~:x 1])))

(deftest qq-seq-2
  (let [x :x]
    (is (= [:x] `[~x]))))

(deftest qq-map
  (let [form :form]
    (is (= '{:expected :form}
           `{:expected ~form}))))

(deftest qq-set
  (let [x :x]
    (is (= '#{:x} `#{~x}))))

(deftest autogensym
  (let [[sym val] (eval `(let [x# 1] (list 'x# x#)))]
    (is (CL:NULL (CL:SYMBOL-PACKAGE sym)))
    (is (= val 1))))

(deftest eval-literal
  (is (= [(+ 1 1)] [2]))
  (is (= {:x (+ 1 1)} {:x 2})))

(deftest no-nest-anons
  (is (thrown? IllegalStateException (read-string "#(#())"))))

(deftest function-literal
  (is (= 1 (CL:FUNCALL #(do %) 1)))
  (is (= 1 (#(do %) 1)))
  (is (= '(1) (#(list %) 1)))
  (is (= '((1 2 3)) (#(list %&) 1 2 3)))
  (is (= '(1 (2 3)) (#(list % %&) 1 2 3))))

(deftest deref-syntax
  (is (= '(clojure.core:deref :x)
         '@:x)))

(deftest test-pre-post-map
  ;; Test that a solitary map in the body is not misinterpreted as a pre-post map.
  (let [f (fn [x] {:x x})]
    (is (= (f 1) {:x 1}))))

(deftest fn-destructure
  (is (= '(1 2 3)
         (CL:FUNCALL (fn [[x y z]] (list x y z))
                     '(1 2 3)))))

(deftest fn-lisp-1
  (is (= -1
         (CL:FUNCALL (fn [x y] (x y))
                     - 1))))

(defmulti factorial identity)
(defmethod factorial 0 [_]  1)
(defmethod factorial :default [num]
  (* num (factorial (dec num))))

(deftest defmulti-identity
  (is (= 1 (factorial 0)))
  (is (= 1 (factorial 1)))
  (is (= 6 (factorial 3)))
  (is (= 5040 (factorial 7))))

(defmulti rand-str
  (fn [] (> (rand) 0.5)))

(defmethod rand-str true
  [] "true")

(defmethod rand-str false
  [] "false")

(deftest defmulti-random
  (dotimes [i 5]
    (is (contains? #{"false" "true"} (rand-str)))))

(deftest quoted-literals
  (is (identical? (first '(nil))   (first (list nil))))
  (is (identical? (first '(true))  (first (list true))))
  (is (identical? (first '(false)) (first (list false)))))

(deftest empty-lazy-seq
  (is (empty? (lazy-seq '())))
  (is (not (seq (lazy-seq '()))))
  (is (not (seq (lazy-seq nil)))))

(deftest lazy-seq-equality
  (is (= (lazy-seq (cons 1 (lazy-seq '(2))))
         '(1 2)))
  (is (not= (lazy-seq (cons 1 (lazy-seq '(2))))
            '(1 3)))
  (is (= (lazy-seq (cons 1 (lazy-seq '(2))))
         (lazy-seq (cons 1 (lazy-seq '(2))))))
  (is (not= (lazy-seq (cons 1 (lazy-seq '(2))))
            (lazy-seq (cons 1 (lazy-seq '(3)))))))

(deftest lazy-seq-unrealized
  (let [tail (lazy-seq (list 2 3))
        seq (lazy-seq (list 1 tail))]
    (is (not (realized? seq)))
    (= '(1) seq)                        ;Force just the first element.
    (is (realized? seq))
    (is (not (realized? tail)))))

(defn squares-odd [n]
  (cons (* n n) (lazy-seq (squares-odd (inc n)))))
(defn squares-even [n]
  (lazy-seq (cons (* n n) (squares-even (inc n)))))

(deftest test-lazy-seq
  (is (= (take 1 (squares-odd 1))
         (take 1 (squares-even 1)))))

(deftest test-cycle
  (is (= '(1 2 3 1 2 3 1 2 3 1)
         (take 10 (cycle '(1 2 3))))))

(deftest test-concat
  (is (= '(1 2 3 4 5 6)
         (concat '(1 2 3) '(4 5 6)))))

(deftest test-take
  (is (= '(1 2 3 4 5 6)
         (take 6 '(1 2 3 4 5 6)))))

(deftest test-repeat
  (is (= '(1 1 1 1 1)
         (repeat 5 1)))
  (is (= '(1 1 1)
         (repeat 3 1))))

(deftest test-filter
  (is (= '(0 2 4 6 8)
         (filter even? (range 10)))))

(deftest test-map
  (is (= (CL:MAP 'CL:LIST - (SERAPEUM:RANGE 5))
         (map - (range 5))))
  (is (= (CL:MAP 'CL:LIST - (SERAPEUM:RANGE 5) (SERAPEUM:RANGE 5 10))
         (map - (range 5) (range 5 10)))))

(deftest test-range
  (is (empty? (take 10 (range 0 0 0))))
  (is (= (CL:MAKE-LIST 5 :INITIAL-ELEMENT 0)
         (take 5 (range 0 10 0))))
  (is (= (SERAPEUM:RANGE 10)
         (take 10 (range)))))

(deftest test-drop
  (is (= [1 2 3 4] (drop -1 [1 2 3 4])))
  (is (= [1 2 3 4] (drop 0 [1 2 3 4])))
  (is (= [3 4] (drop 2 [1 2 3 4])))
  (is (= [] (drop 5 [1 2 3 4]))))

(deftest test-drop-while
  (is (= '(2 4 6)
         (drop-while odd? '(1 3 5 2 4 6)))))

(deftest test-interpose
  (is (= '("one" "," "two" "," "three")
         (interpose "," '("one" "two" "three")))))

(deftest test-group-by
  (let [map (group-by count ["a" "as" "asd" "aa" "asdf" "qwer"])]
    (is (= (count map) 4))))

(deftest doseq-test
  (is (= "123"
         (with-out-str
           (doseq [x '(1 2 3)]
             (pr x)))))
  (is (nil? (doseq [_ '(1)]))))

(deftest doseq-nested
  (is (= "-1-2-3000123"
         (with-out-str
           (doseq [x [-1 0 1]
                   y [1 2 3]]
             (pr (* x y)))))))

(deftest test-assoc-in
  (is (= {:x {:y {:z 1}}}
         (assoc-in {} [:x :y :z] 1))))

(deftest test-macroexpand
  (is (= '(def x (fn x []))
         (macroexpand '(defn x [])))))

(deftest test-name
  (is (= "c" (name 'c)))
  (is (= "c" (name :c)))
  (is (= "c" (name :b/c)))
  (is (= "c" (name 'b/c)))
  (is (= "b/c" (name 'a/b/c)))
  (is (= "b/c" (name :a/b/c))))

(deftest test-floats
  (let [array (floats [])]
    (is (= (CL:ARRAY-ELEMENT-TYPE array) 'CL:DOUBLE-FLOAT)))
  (let [array (floats [1 2 3])]
    (is (= (CL:ARRAY-ELEMENT-TYPE array) 'CL:DOUBLE-FLOAT))
    (is (= array '(1.0 2.0 3.0)))))

(deftest test-reduce
  (is (= 15 (reduce + [1 2 3 4 5])))
  (is (= 0 (reduce + [])))
  (is (= 1 (reduce + [1])))
  (is (= 3 (reduce + [1 2])))
  (is (= 1 (reduce + 1 [])))
  (is (= 6 (reduce + 1 [2 3]))))

(deftest test-reduced
  (is (not (reduced? :x)))
  (is (reduced? (reduced :x)))
  (is (= :x (unreduced (reduced :x))))
  (is (not (reduced? (unreduced (reduced :x)))))

  (is (= 6 (reduce + #{1 2 3})))

  (is (reduced? (reduce + (reduced :x) '())))

  (is (= 6
         (reduce (fn [x y]
                   (when (> y 5)
                     (reduced y)))
                 (range)))))

(deftest test-get-in
  (is (= {:a 1 :b 2}
         (get-in {:a 1 :b 2} nil :nothing))))

(deftest test-persistent!
  (let [x (transient {})]
    (persistent! x)
    (is (thrown? IllegalAccessError (persistent! x)))))

(defprotocol IFoo
  (foo [x])
  (foo [x y])
  (foo [x y z]))

(deftype Foomatic []
  IFoo
  (foo [_] 1)
  (foo [_ _] 2)
  (foo [_ _ _] 3))

(deftest test-polymorphic-defprotocol
  (let [foomatic (Foomatic.)]
    (is (= 1 (foo foomatic)))
    (is (= 2 (foo foomatic 1)))
    (is (= 3 (foo foomatic 1 2)))))

(defprotocol IPersonName
  (person-name [person]))

(deftype Person [name]
  IPersonName
  (person-name [self] name))

(deftest test-deftype-slots
  (let [name "John Q. Person"]
    (is (= name (person-name (Person. name))))))

(deftest test-into
  (is (= [] (into)))
  (is (= [1 2 3] (into [1 2 3])))
  (is (= {:x 1 :y 2} (into {:x 1} {:y 2})))
  (is (= {:x 1} (into {} [[:x 1]])))
  (is (= {:x 1} (into {} [{:x 1}])))
  (is (= [[:x 1] [:y 2]] (into [] {:x 1 :y 2})))
  (is (= '(3 2 1) (into () '(1 2 3))))
  (is (= [1 2 3 4 5 6] (into [1 2 3] '(4 5 6)))))

(deftest unchecked-math
  (= (unchecked-add Integer/MAX_VALUE 0) Integer/MAX_VALUE)
  (= (unchecked-add Integer/MAX_VALUE 1) Integer/MIN_VALUE)
  (= -2 (unchecked-add Integer/MAX_VALUE Integer/MAX_VALUE))
  (= -2 (unchecked-add Long/MAX_VALUE Long/MAX_VALUE))
  (= (unchecked-negate Long/MIN_VALUE)
     Long/MIN_VALUE)
  (= (unchecked-inc Integer/MAX_VALUE)
     Integer/MIN_VALUE)
  (= (unchecked-inc Long/MAX_VALUE)
     Long/MIN_VALUE)
  (= (unchecked-dec Integer/MIN_VALUE)
     (dec Integer/MIN_VALUE))
  (= (unchecked-add-int Integer/MAX_VALUE 1)
     Integer/MIN_VALUE)
  (= (unchecked-add-int Integer/MIN_VALUE -1)
     Integer/MAX_VALUE))

(deftest test-string-replace
  (is (= "lmostAay igPay atinLay"
         (s/replace "Almost Pig Latin" #"\b(\w)(\w+)\b" "$2$1ay"))))

(deftest test-distinct
  (let [l '(:x :y :z 1 2 3 :x :z :y 3 2 1)]
    (is (= (count (set l))
           (count (distinct l))))))

(defrecord Foo [a b])
(defrecord Bar [a b])

(deftest test-absolute-class
  (is (= Foo cloture.tests.Foo))
  (is (= Bar cloture.tests.Bar)))

(deftest test-defrecord
  (is (= (Foo. 1 2) (->Foo 1 2) (map->Foo {:a 1 :b 2})))
  (is (= (hash (->Foo 1 2)) (hash (map->Foo {:a 1 :b 2}))))
  (is (not= (->Foo 1 2) (->Bar 1 2)))
  (is (= '(:a :b) (keys (->Foo 1 2))))
  (is (= '(1 2) (vals (->Foo 1 2))))
  (is (= 1 (get (->Foo 1 2) :a)))
  (is (= true (get (->Foo 1 2) :c true)))
  (is (= 2 (count (->Foo 1 2))))
  (is (= (type (->Foo 1 2))
         (type (assoc (->Foo 1 2) :c 3))))
  (is (= (type (->Foo 1 2))
         (-> (->Foo 1 2)
             (assoc :c 3)
             (dissoc :c)
             type)))
  (is (not= (type (->Foo 1 2))
            (type (dissoc (->Foo 1 2) :b)))))

(deftest test-defrecord-map
  (is (= '([:a 1] [:b 2])
         (map (fn [[k v]] [k v])
              (->Foo 1 2)))))

(deftest test-sorted-map-equality
  (let [m1 (sorted-map-by > 3 -7 5 10 15 20)
        m2 {3 -7, 5 10, 15 20}]
    (is (= m1 m2))))

(deftest test-sorted-map
  (is (= '([:a 28] [:b 35] [:z 0])
         (seq (sorted-map :z 0 :a 28 :b 35))))
  (is (not (= '([:a 28] [:b 35] [:z 0])
              (sorted-map :z 0 :a 28 :b 35)))))

(deftest test-sorted-set
  (is (= [1 2 3] (seq (sorted-set 1 2 3))))
  (is (not (= [1 2 3] (sorted-set 1 2 3)))))

(deftest test-map-not-seq
  (is (not (seq? {:x 1}))))

(deftest test-set-not-seq
  (is (not (seq? #{:x}))))

(deftest test-map-vector-equality
  (let [v1 ["a" "b" "c"]
        v2 {0 "a" 1 "b" 2 "c"}]
    (is (not (= v1 v2)))))

(deftest test-mapcat
  (is (= '(0 1 2 3 4 5 6 7 8 9)
         (mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]]))))

(deftest test-atom
  (let [atm (atom 0)]
    (is (= 0 @atm))
    (is (atom? atm))
    (swap! atm inc)
    (is (= 1 @atm)))
  (let [atm (atom #{})]
    (is (= #{} @atm))
    (swap! atm conj :x)
    (is (= #{:x} @atm))
    (reset! atm 0)
    (is (= 0 @atm))))

(deftest when-let-regression
  (when-let [_ false]
    nil
    (is false)))

(deftest test-if-let
  (if-let [_ false]
    (is false)
    (is true))
  (if-let [_ nil]
    (is false)
    (is true))
  (if-let [_ '()]
    (is true)
    (is false)))

(deftest test-function-equality
  (let [f (fn [x] x)
        g (fn [x] x)]
    (is (= f f))
    (is (not= f g))))

(deftest test-case
  (case :x
    :y (is false)
    :x (is true)))

(deftest test-case-default
  (is (= 1
         (case :z
           :y (is false)
           :x (is true)
           1))))

(deftest test-or
  (is (nil? (or)))
  (is (false? (or false)))
  (is (true? (or true)))
  (is (nil? (or nil)))
  (is (true? (or true false false)))
  (is (true? (or true true true)))
  (is (false? (or false false false)))
  (is (nil? (or nil nil)))
  (is (nil? (or false nil)))
  (is (true? (or true nil)))
  (is (= 42 (or false 42)))
  (is (= 42 (or false 42 9999)))
  (is (= 42 (or 42 9999)))
  (is (false? (or nil false)))
  (is (nil? (or false nil))))

;;; Check that templates work (`are` uses templates internally).

(deftest test-template
  (are [x y] (= x y)
    1 1
    2 2
    3 3))

(deftest test-postwalk-replace
  (is (= (walk/postwalk-replace {:a 1 :b 2} [:a :b])
         [1 2]))
  (is (= (walk/postwalk-replace {:a 1 :b 2} [:a :b :c])
         [1 2 :c]))
  (is (= (walk/postwalk-replace {:a 1 :b 2} [:a :b [:a :b] :c])
         [1 2 [1 2] :c]))
  (is (= (walk/postwalk-replace {:x :X} {:a 1, :b :x, :c 3, :x 4})
         {:X 4, :a 1, :b :X, :c 3})))

(deftest test-for
  (is (= [0 6 12]
         (for [x [0 1 2 3 4 5]
               :let [y (* x 3)]
               :when (even? y)]
           y)))
  (is (= '([1 1 1] [2 4 8] [3 9 27] [4 16 64] [5 25 125])
         (for [x (range 1 6)
               :let [y (* x x)
                     z (* x x x)]]
           [x y z])))
  (is (= '(:c)
         (for [[x y] '([:a 1] [:b 2] [:c 0]) :when (= y 0)]
           x)))
  (is (= '([0 0] [0 1] [0 2])
         (for [x (range 3) :while (not= x 1) y (range 3)]
           [x y]))))

;;; Careful not to return nil when the key is absent.

(deftest test-select-keys
  (is (empty? (select-keys {:a 1} [:b])))
  (is (= {:a 1 :b 2}
         (select-keys {:a 1 :b 2 :d 3} [:a :b :c]))))

(deftest test-parse-integer
  (is (= 234
         (parse-integer "1234x" :start 1 :junk-allowed true))))

;;; A prefix is not an equal. Sequential equality must compare lengths in
;;; both directions, and an empty sequential collection must not swallow
;;; nil, a number, an empty map or an empty set.

(deftest test-sequential-equality-is-length-sensitive
  (is (= '(1 2 3) [1 2 3]))
  (is (not= '(1 2) '(1 2 3)))
  (is (not= '(1 2 3) '(1 2)))
  (is (not= '(1 2 3) [1 2 3 4]))
  (is (not= '(1 2 3 4) [1 2 3]))
  (is (not= [1] [1 2]))
  (is (not= [1 2] [1]))
  (is (not= '([:x 1] [:y 2]) (seq {:x 1 :y 2 :z 3})))
  (is (not= (map inc [1 2]) '(2 3 4))))

(deftest test-empty-collection-equality
  (testing "empty sequential collections are equal to each other"
    (is (= () []))
    (is (= [] ()))
    (is (= (list) []))
    (is (= (rest [1]) []))
    (is (= (rest '(1)) ())))
  (testing "an empty sequential collection is not equal to nil"
    (is (not= () nil))
    (is (not= nil ()))
    (is (not= [] nil))
    (is (not= nil [])))
  (testing "an empty sequential collection is not equal to an empty map or set"
    (is (not= () {}))
    (is (not= [] {}))
    (is (not= () #{}))
    (is (not= [] #{})))
  (testing "an empty sequential collection is not equal to a non-collection"
    (is (not= [] 4))
    (is (not= () 4))
    (is (not= [] ""))
    (is (not= () ""))))

(deftest test-equality-is-symmetric
  (are [x y] (= (= x y) (= y x))
    () nil
    [] nil
    () []
    [] {}
    () #{}
    [] 4
    '(1 2) '(1 2 3)
    '(1 2 3) [1 2 3 4]))

(deftest test-some?
  (is (false? (some? nil)))
  (is (true? (some? false)))
  (is (true? (some? 0)))
  (is (true? (some? []))))

(deftest test-any?
  (is (true? (any? nil)))
  (is (true? (any? 1)))
  (is (true? (any? "x"))))

(deftest test-mapv
  (is (= [2 3 4] (mapv inc [1 2 3])))
  (is (= [] (mapv inc [])))
  (is (vector? (mapv inc [1 2 3])))
  (is (= [5 7 9] (mapv + [1 2 3] [4 5 6]))))

(deftest test-filterv
  (is (= [2 4] (filterv even? [1 2 3 4])))
  (is (= [] (filterv even? [1 3])))
  (is (vector? (filterv even? [1 2]))))

(deftest test-keep
  (is (= '(1 3) (keep (fn [x] (if (odd? x) x nil)) [1 2 3])))
  (is (= '(false) (keep (fn [x] (if (= x 1) false nil)) [1 2])))
  (is (= '() (keep (fn [_] nil) [1 2 3]))))

(deftest test-numeric-predicates
  (testing "number?"
    (is (true? (number? 1)))
    (is (true? (number? 1.5)))
    (is (false? (number? "1")))
    (is (false? (number? nil))))
  (testing "int?"
    (is (true? (int? 1)))
    (is (false? (int? 1.5)))
    (is (false? (int? "1"))))
  (testing "float? and double?"
    (is (true? (float? 1.5)))
    (is (false? (float? 1)))
    (is (true? (double? 1.5)))
    (is (false? (double? 1))))
  (testing "signed integer predicates"
    (is (true? (pos-int? 1)))
    (is (false? (pos-int? 0)))
    (is (false? (pos-int? -1)))
    (is (false? (pos-int? 1.5)))
    (is (true? (neg-int? -1)))
    (is (false? (neg-int? 0)))
    (is (true? (nat-int? 0)))
    (is (true? (nat-int? 1)))
    (is (false? (nat-int? -1)))))

(deftest test-boolean?
  (is (true? (boolean? true)))
  (is (true? (boolean? false)))
  (is (false? (boolean? nil)))
  (is (false? (boolean? 0)))
  (is (false? (boolean? "true"))))

(deftest test-conj-arities
  (is (= [] (conj)))
  (is (= [1] (conj [1])))
  (is (= [1 2] (conj [1] 2)))
  (is (= [1 2 3] (conj [1] 2 3))))

(deftest test-lower-case
  (is (= "ab" (s/lower-case "AB")))
  (is (= "ab1" (s/lower-case "Ab1")))
  (is (= "" (s/lower-case ""))))

(deftest test-symbol-one-arity
  (testing "one argument names an unqualified symbol"
    (is (symbol? (symbol "colour")))
    (is (= "colour" (name (symbol "colour"))))
    (is (= (symbol "colour") (symbol "colour"))))
  (testing "two arguments name a symbol in that namespace"
    (is (symbol? (symbol "clojure.core" "inc")))
    (is (= "inc" (name (symbol "clojure.core" "inc"))))
    (is (not= (symbol "inc") (symbol "clojure.core" "inc")))))

(deftest test-vary-meta
  (testing "the metadata map is passed through the fn"
    (is (= {:a 1 :b 2}
           (meta (vary-meta (with-meta [1 2] {:a 1}) assoc :b 2)))))
  (testing "the value itself is unchanged"
    (is (= [1 2] (vary-meta (with-meta [1 2] {:a 1}) assoc :b 2))))
  (testing "no metadata means the fn sees nil"
    (is (= {:b 2} (meta (vary-meta [1 2] assoc :b 2))))))

(deftest test-ex-info
  (testing "message and data round-trip"
    (let [e (ex-info "boom" {:a 1})]
      (is (= "boom" (ex-message e)))
      (is (= {:a 1} (ex-data e)))))
  (testing "an ex-info is throwable and catchable"
    (is (= "boom" (try (throw (ex-info "boom" {:a 1}))
                       (catch Exception e (ex-message e)))))
    (is (= {:a 1} (try (throw (ex-info "boom" {:a 1}))
                       (catch Exception e (ex-data e))))))
  (testing "ex-data is nil for anything else"
    (is (nil? (ex-data (Exception. "plain"))))
    (is (nil? (ex-data 4)))
    (is (nil? (ex-message 4)))))

(deftest test-keyword-call
  (testing "a keyword looks itself up in a map"
    (is (= 1 (:a {:a 1 :b 2})))
    (is (nil? (:missing {:a 1}))))
  (testing "a second argument is the not-found value"
    (is (= :dflt (:missing {:a 1} :dflt)))
    (is (= 1 (:a {:a 1} :dflt))))
  (testing "a keyword is callable as a higher-order function"
    (is (= [1 2] (mapv :a [{:a 1} {:a 2}])))
    (is (= [{:a 1}] (filterv :a [{:a 1} {:b 2}]))))
  (testing "a keyword built at run time is callable too"
    (is (= [1] (mapv (keyword "a") [{:a 1}])))
    (is (= [nil] (mapv (keyword "b") [{:a 1}]))))
  (testing "a namespaced keyword is callable"
    (is (= 1 (:my.ns/a {:my.ns/a 1})))))

(deftest test-assoc-arities
  (testing "a map takes any number of key/value pairs"
    (is (= {:a 1} (assoc {} :a 1)))
    (is (= {:a 1 :b 2} (assoc {} :a 1 :b 2)))
    (is (= {:a 1 :b 2 :c 3} (assoc {} :a 1 :b 2 :c 3)))
    (is (= {:a 2} (assoc {:a 1} :a 2))))
  (testing "a vector associates by index"
    (is (= [:x 2 3] (assoc [1 2 3] 0 :x)))
    (is (= [:x 2 :z] (assoc [1 2 3] 0 :x 2 :z)))
    (is (= :x (get (assoc [1 2 3] 0 :x) 0))))
  (testing "an index one past the end appends"
    (is (= [1 2 3 4] (assoc [1 2 3] 3 4))))
  (testing "a bad index is an error"
    (is (thrown? Exception (assoc [1 2 3] 9 :oob)))
    (is (thrown? Exception (assoc [1 2 3] :k :v)))))

(deftest test-update
  (is (= {:a 2} (update {:a 1} :a inc)))
  (is (= {:a 3} (update {:a 1} :a + 2))))

(deftest test-string-trim
  (is (= "a b" (s/trim "  a b  ")))
  (is (= "a  " (s/triml "  a  ")))
  (is (= "  a" (s/trimr "  a  ")))
  (is (= "a" (s/trim-newline "a\n")))
  (is (= "a" (s/trim-newline "a\r\n"))))

(deftest test-string-blank?
  (is (true? (s/blank? "")))
  (is (true? (s/blank? "   \t\n")))
  (is (true? (s/blank? nil)))
  (is (false? (s/blank? "a")))
  (is (false? (s/blank? " a "))))

(deftest test-string-capitalize
  (is (= "Foo" (s/capitalize "fOO")))
  (is (= "A" (s/capitalize "a")))
  (is (= "" (s/capitalize ""))))

(deftest test-string-split
  (is (= ["a" "b" "c"] (s/split "a,b,c" #",")))
  (is (= ["a" "b,c"] (s/split "a,b,c" #"," 2)))
  (testing "trailing empty strings are dropped"
    (is (= ["a" "" "b"] (s/split "a,,b,," #","))))
  (is (= ["a" "b"] (s/split-lines "a\nb")))
  (is (= ["a" "b"] (s/split-lines "a\r\nb"))))

(deftest test-string-search
  (is (true? (s/includes? "hello" "ell")))
  (is (false? (s/includes? "hello" "z")))
  (is (= 2 (s/index-of "hello" "l")))
  (is (= 3 (s/last-index-of "hello" "l")))
  (is (nil? (s/index-of "hello" "z"))))

(deftest test-sort
  (is (= [1 2 3] (sort [3 1 2])))
  (is (= [3 2 1] (sort > [3 1 2])))
  (is (= [] (sort [])))
  (is (= ["a" "bb"] (sort-by count ["bb" "a"])))
  (is (= [[1] [1 2]] (sort-by count [[1 2] [1]]))))

(deftest test-frequencies
  (is (= {:a 2 :b 1} (frequencies [:a :b :a])))
  (is (= {} (frequencies []))))

(deftest test-seq-additions
  (is (= [1 3] (vec (take-while odd? [1 3 4 5]))))
  (is (= [4 5] (vec (drop-while odd? [1 3 4 5]))))
  (is (= [[1 2] [3]] (split-at 2 [1 2 3])))
  (is (= [[1 3] [4]] (split-with odd? [1 3 4])))
  (is (nil? (not-empty [])))
  (is (= [1] (not-empty [1])))
  (is (= [1 :a 2 :b] (vec (interleave [1 2] [:a :b]))))
  (is (= [[0 :a] [1 :b]] (vec (map-indexed (fn [i x] [i x]) [:a :b]))))
  (is (= [[1 2] [3 4]] (vec (partition 2 [1 2 3 4 5]))))
  (is (= [[1 2] [3]] (vec (partition-all 2 [1 2 3]))))
  (is (true? (distinct? 1 2 3)))
  (is (false? (distinct? 1 2 2))))

(deftest test-parse
  (is (= 42 (parse-long "42")))
  (is (= -7 (parse-long "-7")))
  (is (nil? (parse-long "4x")))
  (is (= 1.5 (parse-double "1.5")))
  (is (nil? (parse-double "x")))
  (is (true? (parse-boolean "true")))
  (is (false? (parse-boolean "false")))
  (is (nil? (parse-boolean "maybe"))))

(deftest test-compare-and-set
  (let [a (atom 1)]
    (is (true? (compare-and-set! a 1 2)))
    (is (= 2 @a))
    (is (false? (compare-and-set! a 9 3)))
    (is (= 2 @a))))

(deftest test-namespace
  (is (= "a" (namespace :a/b)))
  (is (nil? (namespace :b))))

(deftest test-transducers
  (testing "into with a transducer"
    (is (= [2 3 4] (into [] (map inc) [1 2 3])))
    (is (= [1 3] (into [] (filter odd?) [1 2 3])))
    (is (= [2] (into [] (remove odd?) [1 2 3])))
    (is (= {1 1 3 3} (into {} (keep (fn [x] (if (odd? x) [x x] nil))) [1 2 3]))))
  (testing "transduce"
    (is (= 9 (transduce (map inc) + 0 [1 2 3])))
    (is (= 9 (transduce (map inc) + [1 2 3])))
    (is (= 4 (transduce (filter odd?) + [1 2 3]))))
  (testing "the sequence arities still work"
    (is (= [2 3 4] (vec (map inc [1 2 3]))))
    (is (= [1 3] (vec (filter odd? [1 2 3]))))
    (is (= [2] (vec (remove odd? [1 2 3]))))
    (is (= [1 3] (vec (keep (fn [x] (if (odd? x) x nil)) [1 2 3]))))
    (is (= [2] (vec (into [] [2]))))))
