(ns petrushka.terms.set-test
  (:require [hyperfiddle.rcf :refer [tests]]
            [petrushka.api :as api]
            [petrushka.main :as main :refer [?> fresh satisfy]]
            [petrushka.protocols :as protocols]
            [petrushka.types :as types]
            [petrushka.utils.test :refer [throws? only-val]]
            [clojure.set :as set]))

(tests
 #{4 5 6} :=
 (only-val
  (satisfy
   (=
    (api/bind (range 100) (fresh))
    (set/intersection
     #{1 2 3 4 5 6}
     #{4 5 6 7 8 9})))))