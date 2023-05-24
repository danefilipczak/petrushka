(ns petrushka.test.terms.core
  (:require [petrushka.api :as api :refer [satisfy fresh]]
           [hyperfiddle.rcf :refer [tests]]))

(def only-val (comp first vals))

(tests 
 "addition"
 (only-val (satisfy (= (+ 1 (fresh)) 3))) := 2
 )