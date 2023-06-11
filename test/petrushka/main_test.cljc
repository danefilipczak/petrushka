(ns petrushka.main-test
  (:require [hyperfiddle.rcf :refer [tests]]
            [petrushka.protocols :as protocols]
            [petrushka.main :as main :refer [bind ?> fresh satisfy]]
            [petrushka.types :as types]
            [petrushka.utils.test :refer [throws?]]
            [petrushka.api :as api]))


(tests "conjunction does not stack overflow on many clauses"
 false := (throws?
           (let [n 15]
             (->> (for [x (range n)
                        y (range n)
                        :let [a (fresh)
                              b (fresh)]]
                    (?> (and
                         (= a x)
                         (= b y)
                         (= (+ a b) (+ x y)))))
                  (apply main/conjunction)))))