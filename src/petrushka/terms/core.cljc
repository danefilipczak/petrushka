(ns petrushka.terms.core
  (:require [petrushka.protocols :as protocols]
            [petrushka.types :as types]
            [petrushka.utils.symbol :as symbols]
            [petrushka.utils.string :refer [>>]]
            [petrushka.api :as api]))

(defrecord TermPlus [argv]
  protocols/IExpress
  (write [_self] (apply list '+ (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (repeat {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "+" (map protocols/translate (:argv self)))))

(defmethod protocols/rewrite* + [_] ->TermPlus)

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

(defmethod protocols/rewrite* >= [_] ->TermGreaterThanOrEqualTo)

(defrecord TermNot [argv]
  protocols/IExpress
  (write [_self] (apply list 'not (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Bool self}])
  (decisions [_self] (protocols/decisions (first argv)))
  (bindings [_self] (protocols/bindings (first argv)))
  (validate [self] (api/validate-domains self))
  (translate [_self] (>> {:arg (protocols/translate (first argv))} "(not {{arg}})")))

(defmethod protocols/rewrite* not [_] ->TermNot)

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

(defmethod protocols/rewrite* = [_] ->TermEquals)

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

(defmethod protocols/rewrite* contains? [_] ->TermContains)

(defrecord TermModulo [argv]
  protocols/IExpress
  (write [_self] (apply list 'mod (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Numeric self} {types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (apply api/translate-binary-operation "mod" (map protocols/translate argv))))

(defmethod protocols/rewrite* mod [_] ->TermModulo)