(ns petrushka.test.terms.core
  (:require [petrushka.main :as main :refer [satisfy fresh ?>]]
           [hyperfiddle.rcf :refer [tests]]
           [petrushka.protocols :as protocols]
            [petrushka.types :as types]))

(def only-val (comp first vals))

(tests 
 "addition"
 (only-val (satisfy (= (+ 1 (fresh)) 3))) := 2
 )

(tests "equality"
       (count (only-val (protocols/decisions (?> (= (fresh) 1)))))
       := 1

       (count (only-val (protocols/decisions (?> (= (fresh) #{})))))
       := 1

       (count (only-val (protocols/decisions (?> (= (fresh) (fresh))))))
       := (count types/all-decision-types))


(comment
  
  (protocols/codomain 1)
  )