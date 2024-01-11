(ns petrushka.terms.set
  (:require [petrushka.protocols :as protocols]
            [petrushka.types :as types]
            [petrushka.api :as api]
            [petrushka.set :as p.set]
            [clojure.set :as set]))

(defrecord TermIntersection [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/intersection (map protocols/write argv)))
  (codomain [self] {types/Set self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "intersect" (map protocols/translate (:argv self)))))

(defmethod protocols/rewrite-function set/intersection [_] ->TermIntersection)

(defrecord TermDifference [argv]
           protocols/IExpress
           (write [_self] (apply list 'clojure.set/difference (map protocols/write argv)))
           (codomain [self] {types/Set self})
           (domainv [self] (repeat {types/Set self}))
           (decisions [self] (api/unify-argv-decisions self))
           (bindings [self] (api/unify-argv-bindings self))
           (validate [self] (api/validate-domains self))
           (translate [self] (api/translate-nary-operation "diff" (map protocols/translate (:argv self)))))

(defmethod protocols/rewrite-function set/difference [_] ->TermDifference)

(defrecord TermSymDiff [argv]
           protocols/IExpress
           (write [_self] (apply list 'petrushka.set/sym-diff (map protocols/write argv)))
           (codomain [self] {types/Set self})
           (domainv [self] (repeat {types/Set self}))
           (decisions [self] (api/unify-argv-decisions self))
           (bindings [self] (api/unify-argv-bindings self))
           (validate [self] (api/validate-domains self))
           (translate [self] (api/translate-nary-operation "symdiff" (map protocols/translate (:argv self)))))

(defmethod protocols/rewrite-function p.set/sym-diff [_] ->TermSymDiff)

(defrecord TermUnion [argv]
           protocols/IExpress
           (write [_self] (apply list 'clojure.set/union (map protocols/write argv)))
           (codomain [self] {types/Set self})
           (domainv [self] (repeat {types/Set self}))
           (decisions [self] (api/unify-argv-decisions self))
           (bindings [self] (api/unify-argv-bindings self))
           (validate [self] (api/validate-domains self))
           (translate [self] (api/translate-nary-operation "union" (map protocols/translate (:argv self)))))

(defmethod protocols/rewrite-function set/union [_] ->TermUnion)

(defrecord TermSubset [argv]
           protocols/IExpress
           (write [_self] (apply list 'clojure.set/subset? (map protocols/write argv)))
           (codomain [self] {types/Bool self})
           (domainv [self] (repeat {types/Set self}))
           (decisions [self] (api/unify-argv-decisions self))
           (bindings [self] (api/unify-argv-bindings self))
           (validate [self] (api/validate-domains self))
           (translate [self] (api/translate-nary-operation "subset" (map protocols/translate (:argv self)))))

(defmethod protocols/rewrite-function set/subset? [_] ->TermSubset)

(defrecord TermSuperset [argv]
           protocols/IExpress
           (write [_self] (apply list 'clojure.set/superset? (map protocols/write argv)))
           (codomain [self] {types/Bool self})
           (domainv [self] (repeat {types/Set self}))
           (decisions [self] (api/unify-argv-decisions self))
           (bindings [self] (api/unify-argv-bindings self))
           (validate [self] (api/validate-domains self))
           (translate [self] (api/translate-nary-operation "superset" (map protocols/translate (:argv self)))))

(defmethod protocols/rewrite-function set/superset? [_] ->TermSuperset)