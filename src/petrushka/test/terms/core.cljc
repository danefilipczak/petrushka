(ns petrushka.test.terms.core
  (:require [petrushka.main :as main :refer [satisfy fresh ?>]]
           [hyperfiddle.rcf :refer [tests]]
           [petrushka.protocols :as protocols]
            [petrushka.types :as types]))

(def only-val (comp first vals))

(tests 
 "+"
 (only-val (satisfy (= (+ 1 (fresh)) 3))) := 2
 )

(tests "="
  (count (only-val (protocols/decisions (?> (= (fresh) 1)))))
  := 1

  (count (only-val (protocols/decisions (?> (= (fresh) #{})))))
  := 1

  (count (only-val (protocols/decisions (?> (= (fresh) (fresh))))))
  := (count types/all-decision-types))

(tests "when"
  (let [a (fresh)]
    (get
     (satisfy
      (?> (when true (= a 3))))
     a)
    := 3

    (not= 3 (get
             (satisfy
              (?> (when false (= a 3))))
             a))
    := true))

(tests "not"
  (let [a (fresh)]
    (not=
     1
     (get
      (satisfy (when (not true)
                 (= a 1)))
      a))
    := true))


