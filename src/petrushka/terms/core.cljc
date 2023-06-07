(ns petrushka.terms.core
  (:require [petrushka.protocols :as protocols]
            [petrushka.types :as types]
            [petrushka.utils.symbol :as symbols]
            [petrushka.utils.string :refer [>>]]
            [petrushka.api :as api]
            [clojure.spec.alpha :as s]))

(defrecord TermPlus [argv]
  protocols/IExpress
  (write [_self] (apply list '+ (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (repeat {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "+" (map protocols/translate (:argv self)))))

(defmethod protocols/rewrite-function + [_] ->TermPlus)

(defrecord TermAnd [argv]
  protocols/IExpress
  (write [_self] (apply list 'and (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Bool self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "/\\" (map protocols/translate (:argv self)))))

(defmethod 
  protocols/rewrite-macro 
  (symbols/fully-qualify-symbol 'and)
  [_]
  ->TermAnd)

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

(defrecord TermGreaterThanOrEqualTo [argv]
  protocols/IExpress
  (write [_self] (apply list '>= (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-comparator self ">=" ->TermGreaterThanOrEqualTo)))

(defmethod protocols/rewrite-function >= [_] ->TermGreaterThanOrEqualTo)

(defrecord TermNot [argv]
  protocols/IExpress
  (write [_self] (apply list 'not (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Bool self}])
  (decisions [_self] (protocols/decisions (first argv)))
  (bindings [_self] (protocols/bindings (first argv)))
  (validate [self] (api/validate-domains self))
  (translate [_self] (>> {:arg (protocols/translate (first argv))} "(not {{arg}})")))

(defmethod protocols/rewrite-function not [_] ->TermNot)

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
  (translate [self] (api/translate-comparator self "=" ->TermEquals)))

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

(defrecord TermModulo [argv]
  protocols/IExpress
  (write [_self] (apply list 'mod (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Numeric self} {types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (apply api/translate-binary-operation "mod" (map protocols/translate argv))))

(defmethod protocols/rewrite-function mod [_] ->TermModulo)