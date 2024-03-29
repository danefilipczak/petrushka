(ns petrushka.terms.introduced
  (:require [petrushka.protocols :as protocols]
            [petrushka.types :as types]
            [petrushka.utils.string :refer [>>]]
            [petrushka.api :as api]
            [petrushka.utils.symbol :as symbols]))

(defrecord TermForAll [bind-sym argv]
  protocols/IExpress
  (write [_self] (let [[local-decision set-expr constraint-expr] argv]
                   (list
                    'forall
                    [bind-sym
                     (protocols/write set-expr)]
                    (clojure.walk/postwalk
                     (fn [e]
                       (if
                        (= e (protocols/write local-decision))
                         bind-sym
                         e))
                     (protocols/write constraint-expr)))))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Numeric self} {types/Set self} {types/Bool self}])
  (decisions [self] (dissoc
                      (api/unify-argv-decisions self)
                      (first argv)))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (>>
                    {:local-decision (protocols/translate (first argv))
                     :set-expr (protocols/translate (second argv))
                     :constraint-expr (protocols/translate (last argv))} 
                     "( forall ( {{local-decision}} in {{set-expr}} )( {{constraint-expr}} ) )")))

(defn forall [& args] :no-op)

(defmethod protocols/rewrite-function forall [_] 
  (fn [[bind-sym argv]]
    (->TermForAll bind-sym argv)))

(defrecord TermForSet [bind-sym argv]
  protocols/IExpress
  (write [_self] (let [[local-decision set-expr generator-expr] argv]
                   (list
                    'for-set
                    [bind-sym
                     (protocols/write set-expr)]
                    (clojure.walk/postwalk
                     (fn [e]
                       (if
                        (= e (protocols/write local-decision))
                         bind-sym
                         e))
                     (protocols/write generator-expr)))))
  (codomain [self] {types/Set self})
  (domainv [self] [{types/Numeric self} {types/Set self} {types/Numeric self}])
  (decisions [self] (dissoc
                     (api/unify-argv-decisions self)
                     (first argv)))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (>>
                     {:local-decision (protocols/translate (first argv))
                      :set-expr (protocols/translate (second argv))
                      :generator-expr (protocols/translate (last argv))}
                     "{ {{generator-expr}} | {{local-decision}} in {{set-expr}} }")))

(defn for-set [& args] :no-op)

(defmethod protocols/rewrite-function for-set [_]
  (fn [[bind-sym argv]]
    (->TermForSet bind-sym argv)))



