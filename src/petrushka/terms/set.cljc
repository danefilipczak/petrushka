(ns petrushka.terms.set
  (:require [petrushka.protocols :as protocols]
            [petrushka.types :as types]
            [petrushka.api :as api]))

(defrecord TermIntersection [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/intersection (map protocols/write argv)))
  (codomain [self] {types/Set self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "intersect" (map protocols/translate (:argv self)))))

(defmethod protocols/rewrite* clojure.set/intersection [_] ->TermIntersection)