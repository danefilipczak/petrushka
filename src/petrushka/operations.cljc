(ns petrushka.operations
  (:require [hyperfiddle.rcf :refer [tests]]))

(def all
  {:-> [:boolean [1] [:boolean]] ;;left implies right
   :<-> [:boolean [1] [:boolean]] ;;mutual implication... all must be true, or false
   :not [:boolean [1 1] [:boolean]]
   :true? [:boolean [1 1] [:boolean]]
   :false? [:boolean [1 1] [:boolean]]
   :or [:boolean [1] [:boolean]]
   :and [:boolean [1] [:boolean]]
   :xor [:boolean [2 2] [:boolean]] ;; arguments must differ
   :+ [:number [2] [:number]] ;; high airity is optional, assumed to be infinite.
   :- [:number [2] [:number]]
   :in [:boolean [2 2] [:number :set]]
   :set= [:boolean [2] [:set]]
   := [:boolean [1] [:number]]
   :> [:boolean [1] [:number]]
   :< [:boolean [1] [:number]]
   :>= [:boolean [1] [:number]]
   :<= [:boolean [1] [:number]]
   :if [:boolean [3 3] [:boolean]]}) ;; the rightmost airity repeats

(tests
 (doall
  (for [[_op [_ airity-bounds arg-types]] all]
    (when (= 2 (count arg-types)) ;; the presence of both a left and right args type implies that it is a binary operator and the airity must be no more or less than 2
      (tests airity-bounds := [2 2])))))