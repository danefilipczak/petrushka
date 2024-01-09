(ns petrushka.terms.core
  (:require [clojure.spec.alpha :as s]
            [petrushka.api :as api]
            [petrushka.protocols :as protocols]
            [petrushka.types :as types]
            [petrushka.utils.string :refer [>>]]
            [petrushka.utils.symbol :as symbols]))

(defn translation-error! [self]
  (throw
   (ex-info
    "This isn't expected to happen because expansion should always be applied before translation."
    {:self self})))

(defrecord TermPlus [argv]
  protocols/IExpand
  (expand [self] (reduce
                  (fn [acc curr]
                    (api/dither (+ acc curr)))
                  (:argv self)))
  protocols/IExpress 
  (write [_self] (apply list '+ (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (repeat {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "+" (map protocols/translate (:argv self)))))

(defmethod protocols/rewrite-function + [_] ->TermPlus)

(defrecord TermProduct [argv]
  protocols/IExpress
  (write [_self] (apply list '* (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (repeat {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "*" (map protocols/translate (:argv self)))))

(defmethod protocols/rewrite-function * [_] ->TermProduct)

(defrecord TermMinus [argv]
  protocols/IExpress
  (write [_self] (apply list '- (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (repeat {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "-" (map protocols/translate (:argv self)))))

(defmethod protocols/rewrite-function - [_] ->TermMinus)

(defrecord TermDivide [argv]
  protocols/IExpress
  (write [_self] (apply list '/ (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (repeat {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "div" (map protocols/translate (:argv self)))))

(defmethod protocols/rewrite-function / [_] ->TermDivide)

(defrecord TermInc [argv]
  protocols/IExpress
  (write [_self] (apply list 'inc (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (protocols/translate 
                     (api/dither (+ (first argv) 1)))))

(defmethod protocols/rewrite-function inc [_] ->TermInc)

(defrecord TermDec [argv]
  protocols/IExpress
  (write [_self] (apply list 'dec (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (protocols/translate
                     (api/dither (- (first argv) 1)))))

(defmethod protocols/rewrite-function dec [_] ->TermDec)

(defrecord TermEven? [argv]
  protocols/IExpand
  (expand [self] (api/dither (= (mod (first argv) 2) 0)))
  protocols/IExpress
  (write [_self] (apply list 'even? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (translation-error! self)))

(defmethod protocols/rewrite-function even? [_] ->TermEven?)

(defrecord TermOdd? [argv]
  protocols/IExpand
  (expand [self] (api/dither (= (mod (first argv) 2) 1)))
  protocols/IExpress
  (write [_self] (apply list 'odd? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (translation-error! self)))

(defmethod protocols/rewrite-function odd? [_] ->TermOdd?)

(defn to-literal-array [elements]
      (apply str (concat ["["] (interpose ", " elements) ["]"])))

(defrecord TermMax [argv]
  protocols/IExpress
  (write [_self] (apply list 'max (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (take (count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (str "max(" (to-literal-array (map protocols/translate (:argv self))) ")")))

(defmethod protocols/rewrite-function max [_] ->TermMax)

(defrecord TermMin [argv]
  protocols/IExpress
  (write [_self] (apply list 'min (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (take (count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (str "min(" (to-literal-array (map protocols/translate (:argv self))) ")")))

(defmethod protocols/rewrite-function min [_] ->TermMin)

(defrecord TermTrue? [argv]
  protocols/IExpress
  (write [_self] (apply list 'true? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Bool self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (protocols/translate
                     (api/dither (= (first argv) true)))))

(defmethod protocols/rewrite-function true? [_] ->TermTrue?)

(defrecord TermFalse? [argv]
  protocols/IExpress
  (write [_self] (apply list 'false? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Bool self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (protocols/translate
                     (api/dither (= (first argv) false)))))

(defmethod protocols/rewrite-function false? [_] ->TermFalse?)

(defrecord TermAnd [argv] 
  protocols/IExpand
  (expand [self] (apply api/conjunction (:argv self)))
  protocols/IExpress
  (write [_self] (apply list 'and (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Bool self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "/\\" (map protocols/translate (:argv self)))))

(defn conjunctive? [x] (= (type x) TermAnd))

(defmethod 
  protocols/rewrite-macro 
  (symbols/fully-qualify-symbol 'and)
  [_]
  ->TermAnd)

(defrecord TermOr [argv]
  protocols/IExpand
  (expand [self] (apply api/disjunction (:argv self)))
  protocols/IExpress
  (write [_self] (apply list 'or (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Bool self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "\\/" (map protocols/translate (:argv self)))))

(defmethod 
  protocols/rewrite-macro 
  (symbols/fully-qualify-symbol 'or)
  [_]
  ->TermOr)

(defrecord TermWhen [argv]
  protocols/IExpress
  (write [_self] (apply list 'when (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take 2 (repeat {types/Bool self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (apply 
                     api/translate-binary-operation 
                     "->" 
                     (map protocols/translate (:argv self)))))

(defmethod 
  protocols/rewrite-macro 
  (symbols/fully-qualify-symbol 'when)
  [_]
  ->TermWhen)

(defrecord TermGreaterThan [argv]
  protocols/IExpress
  (write [_self] (apply list '> (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take (count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-comparator self ">" >)))

(defmethod protocols/rewrite-function > [_] ->TermGreaterThan)

(defrecord TermLessThan [argv]
  protocols/IExpress
  (write [_self] (apply list '< (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take (count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-comparator self "<" <)))

(defmethod protocols/rewrite-function < [_] ->TermLessThan)

(defrecord TermGreaterThanOrEqualTo [argv]
  protocols/IExpand
  (expand [self] (case (count argv)
                   1 true
                   2 self
                   (reduce
                    (fn [acc [x y]]
                      (api/dither
                       (and acc (>= x y))))
                    true
                    (partition 2 1 argv))))
  protocols/IExpress
  (write [_self] (apply list '>= (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take (count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-comparator self ">=" >=)))

(defmethod protocols/rewrite-function >= [_] ->TermGreaterThanOrEqualTo)

(defrecord TermLessThanOrEqualTo [argv]
  protocols/IExpress
  (write [_self] (apply list '<= (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take (count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-comparator self "<=" <=)))

(defmethod protocols/rewrite-function <= [_] ->TermLessThanOrEqualTo)

(defrecord TermNot [argv]
  protocols/IExpress
  (write [_self] (apply list 'not (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Bool self}])
  (decisions [_self] (api/cacheing-decisions (first argv)))
  (bindings [_self] (protocols/bindings (first argv)))
  (validate [self] (api/validate-domains self))
  (translate [_self] (>> {:arg (protocols/translate (first argv))} "(not {{arg}})")))

(defmethod protocols/rewrite-function not [_] ->TermNot)

(defmethod protocols/rewrite-function not= [_] 
  (fn [argv]
    (api/dither (not (apply = argv)))))

(defrecord TermEquals [argv]
  protocols/IExpress
  (write [_self] (apply list '= (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take
                   (count argv)
                   (repeat
                    (zipmap
                     (->> argv
                          (map protocols/codomain)
                          (sort-by count)
                          first
                          keys)
                     (repeat self)))))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self]
    (when (empty? (->> (:argv self)
                       (map (comp set keys protocols/codomain))
                       (apply clojure.set/intersection)))
      (throw (ex-info "equality testing requires consistent types" {})))
    self)
  (translate [self] (api/translate-comparator self "=" =)))

(defmethod protocols/rewrite-function = [_] ->TermEquals)

(defn condititonal-return-exprs [self]
  (->> (:argv self)
       (partition-all 2)
       (map (fn [test-expr-pair]
              (case (count test-expr-pair)
                2 (last test-expr-pair)
                1 (first test-expr-pair))))))

(defn conditional-codomain [self]
  {:post [(s/valid? ::api/domain %)]}
  (zipmap (->> (condititonal-return-exprs self)
               (map protocols/codomain)
               (sort-by count)
               first
               keys)
          (repeat self)))

(defn conditional-domainv [self]
  (let [return-domain (conditional-codomain self)]
    (->> (:argv self)
         (partition-all 2)
         (mapcat (fn [test-expr-pair]
                   (case (count test-expr-pair)
                     2 [{types/Bool self} return-domain]
                     1 [return-domain]))))))

(defn translate-conditional [self]
  (apply
   str
   (concat
    ["("]
    (->> (:argv self)
         (partition-all 2)
         (interleave (range))
         (partition 2)
         (mapcat (fn [[i [test-or-expr expr :as test-expr-pair]]]
                   (case (count test-expr-pair)
                     2 [(if (zero? i) "if " " elseif ")
                        (protocols/translate test-or-expr) 
                        " then " 
                        (protocols/translate expr)]
                     1 [" else " (protocols/translate test-or-expr)]))))
    [" endif)"])))

(defrecord TermIf [argv]
  protocols/IExpress
  (write [_self] (apply list 'if (map protocols/write argv)))
  (codomain [self] (conditional-codomain self))
  (domainv [self] (conditional-domainv self))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self]
            (when (empty? (->> (condititonal-return-exprs self)
                               (map (comp set keys protocols/codomain))
                               (apply clojure.set/intersection)))
              (throw (ex-info "if requires consistent types in its return expressions" {})))
            (api/validate-domains self))
  (translate [self] (translate-conditional self)))

(defmethod protocols/rewrite-symbol 'if [_]
  (fn if-constructor [& args]
    (->TermIf (vec args))))

(defrecord TermCond [argv]
  protocols/IExpress
  (write [_self] (apply list 'cond (map protocols/write argv)))
  (codomain [self] (conditional-codomain self))
  (domainv [self] (conditional-domainv self))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self]
            (when (empty? (->> (condititonal-return-exprs self)
                               (map (comp set keys protocols/codomain))
                               (apply clojure.set/intersection)))
              (throw (ex-info "cond requires consistent types in its return expressions" {})))
            (api/validate-domains self))
  (translate [self] (translate-conditional self)))

(defmethod 
  protocols/rewrite-macro 
  (symbols/fully-qualify-symbol 'cond)
  [_]
  (fn [argv]
    (let [penultimate-term (get argv (- (count argv) 2))]
      (when-not (contains? #{:else :default} penultimate-term)
        (throw (ex-info "cond requires a default test/expression pair, expressed with :else or :default as the test" {})))
      (->TermCond (-> (drop-last 2 argv)
                      vec
                      (conj (last argv)))))))

(defrecord TermContains [argv]
  protocols/IExpress
  (write [_self] (apply list 'contains? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Set self} {types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-binary-operation
                     "in"
                     (protocols/translate (second (:argv self)))
                     (protocols/translate (first (:argv self))))))

(defmethod protocols/rewrite-function contains? [_] ->TermContains)

(defrecord TermPos? [argv]
  protocols/IExpress
  (write [_self] (apply list 'pos? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (protocols/translate
     (api/dither
      (> (first argv) 0)))))

(defmethod protocols/rewrite-function pos? [_] ->TermPos?)

(defrecord TermNeg? [argv]
  protocols/IExpress
  (write [_self] (apply list 'neg? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (protocols/translate
     (api/dither
      (< (first argv) 0)))))

(defmethod protocols/rewrite-function neg? [_] ->TermNeg?)

(defrecord TermZero? [argv]
  protocols/IExpress
  (write [_self] (apply list 'zero? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (protocols/translate
     (api/dither
      (= (first argv) 0)))))

(defmethod protocols/rewrite-function zero? [_] ->TermZero?)

(defrecord TermMod [argv]
  protocols/IExpand
  (expand [self] (api/dither
                  (let [n (first argv)
                        d (second argv)
                        m (api/dither (rem n d))]
                    (if (or (zero? m) (= (pos? n) (pos? d)))
                      m
                      (+ m d)))))
  protocols/IExpress
  (write [_self] (apply list 'mod (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Numeric self} {types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (translation-error! self)))

(defmethod protocols/rewrite-function mod [_] ->TermMod)

(defrecord TermRem [argv]
  protocols/IExpress
  (write [_self] (apply list 'rem (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Numeric self} {types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (apply api/translate-binary-operation "mod" (map protocols/translate argv))))

(defmethod protocols/rewrite-function rem [_] ->TermRem)

(defrecord TermCount [argv]
  protocols/IExpress
  (write [_self] (apply list 'count (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Set self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (>> {:set (protocols/translate (first (:argv self)))} "(card({{set}}))" )))

(defmethod protocols/rewrite-function count [_] ->TermCount)