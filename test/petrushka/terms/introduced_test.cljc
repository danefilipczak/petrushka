(ns petrushka.terms.introduced-test
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
               (= 5 (mod a 12))))]
    (get res x)) := #{65 77 41 89 29 17 5 53}

  (let [cluster-free (fn [set-decision]
                       (main/?>
                        (main/forall [a (api/bind (range 12) set-decision)]
                          (when (contains? set-decision (mod (+ a 1) 12))
                            (not (contains? set-decision (mod (+ a 2) 12)))))))
        x (main/fresh)
        res (main/satisfy
             (cluster-free x))
        validate (fn [s]
                   (every?
                    true?
                    (for [e s]
                      (if (contains? s (mod (+ e 1) 12))
                        (not (contains? s (mod (+ e 2) 12)))
                        true))))]
    (validate (get res x)))
  := true)