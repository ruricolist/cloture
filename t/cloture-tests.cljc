(ns cloture.tests
  (:require [clojure.test :refer [deftest is]]))

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
  (is (not= 1 -1)))

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

(deftest read-nothing
  (is (= '(1 2) '(1 2 #_3)))
  (is (= '(1) '(1 #_#_2 3))))

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
  (is (not (identical? 'clojure.string:starts-with? 'starts-with?)))
  (is (= 'clojure.string:starts-with? 'starts-with?))
  (is (= 1 (get {'clojure.string:starts-with? 1}
                'starts-with?))))

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

(deftest fn-destructure
  (is (= '(1 2 3)
         (CL:FUNCALL (fn [[x y z]] (list x y z))
                     '(1 2 3)))))

(deftest fn-lisp-1
  (is (= -1
         (CL:FUNCALL (fn [x y] (x y))
                     - 1))))
