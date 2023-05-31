(ns petrushka.test.terms.introduced
  (:require [petrushka.protocols :as protocols]
            [petrushka.types :as types]
            [petrushka.api :as api]
            [hyperfiddle.rcf :refer [tests]]
            [petrushka.main :as main]
            [petrushka.utils.test :refer [throws?]]
            [petrushka.utils.symbol :as symbols]))


(tests "forall"

       (tests "internal decision is validated as numeric"
              (throws?
               (main/forall [a (main/fresh)] (= a #{})))
              := true)


       (tests "internal decision is validated as numeric"
              (throws?
               (main/forall [a (main/fresh)] (contains? a 1)))
              := true)

       (tests "internal decision is hidden from external retrieval"
              (count
               (protocols/decisions
                (main/forall [a (main/fresh)] (= a 1))))
              := 1)

       (let [x (main/fresh)
             res (main/satisfy
                  (main/forall [a (api/bind (range 100) x)]
                               (main/?> (= 5 (mod a 12)))))]
         (get res x)) := #{65 77 41 89 29 17 5 53})