(ns petrushka.terms.core
  (:require [petrushka.protocols :as protocols]
            [petrushka.types :as types]
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