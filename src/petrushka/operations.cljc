(ns petrushka.operations
  (:require [hyperfiddle.rcf :refer [tests]]))

(def all
  {:when [:boolean [2 2] [:boolean]]
   :iff [:boolean [2] [:boolean]]
   :not [:boolean [1 1] [:boolean]]
   :true? [:boolean [1 1] [:boolean]]
   :false? [:boolean [1 1] [:boolean]]
   :or [:boolean [1] [:boolean]]
   :and [:boolean [1] [:boolean]]
   :xor [:boolean [2] [:boolean]]
   :+ [:number [2] [:number]]
   :- [:number [2] [:number]]
   :contains? [:boolean [2 2] [:set :number]]
   :set= [:boolean [2] [:set]]
   := [:boolean [1] [:number]]
   :> [:boolean [1] [:number]]
   :< [:boolean [1] [:number]]
   :>= [:boolean [1] [:number]]
   :<= [:boolean [1] [:number]]
   :if [:boolean [3 3] [:boolean]]}) 

(tests
 (doall
  (for [[_op [_ airity-bounds arg-types]] all]
    (when (= 2 (count arg-types)) ;; the presence of both a left and right args type implies that it is a binary operator and the airity must be no more or less than 2
      (tests airity-bounds := [2 2])))))