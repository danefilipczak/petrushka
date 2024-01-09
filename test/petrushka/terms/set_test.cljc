(ns petrushka.terms.set-test
  (:require [hyperfiddle.rcf :refer [tests]]
            [petrushka.main :as main :refer [bind ?> fresh fresh-set satisfy]]
            [petrushka.protocols :as protocols]
            [petrushka.types :as types]
            [petrushka.utils.test :refer [throws? only-val]]
            [clojure.set :as set]))

(tests
 "intersection"
  #{4 5 6} :=
  (let [res (fresh-set (range 12))
        a (fresh-set (range 12))
        b (fresh-set (range 12))]
       (get 
        (satisfy
         (and (= a #{1 2 3 4 5 6})
              (= b #{4 5 6 7 8 9})
              (= res (set/intersection a b))))
        res)))

(tests
 "difference"
  #{1 2 3} :=
  (let [res (fresh-set (range 12))
        a (fresh-set (range 12))
        b (fresh-set (range 12))]
       (get
        (satisfy
         (and (= a #{1 2 3 4 5 6})
              (= b #{4 5 6 7 8 9})
              (= res (set/difference a b))))
        res)))

(tests
 "union"
  #{1 2 3 4 5 6 7 8 9} :=
  (let [res (fresh-set (range 12))
        a (fresh-set (range 12))
        b (fresh-set (range 12))]
       (get
        (satisfy
         (and (= a #{1 2 3 4 5 6})
              (= b #{4 5 6 7 8 9})
              (= res (set/union a b))))
        res)))

(tests
 "subset?"
  false :=
  (let [res (fresh "res")
        a (fresh-set (range 12))
        b (fresh-set (range 12))]
       (get
        (satisfy
         (and (= a #{1 2 3 4 5 6})
              (= b #{4 5 6 7 8 9})
              (= res (set/subset? a b))))
        res))
  
  true :=
  (let [res (fresh "res")
        a (fresh-set (range 12))
        b (fresh-set (range 12))]
       (get
        (satisfy
         (and (= a #{1 2 3})
              (= b #{1 2 3 4 5 6 7 8 9})
              (= res (set/subset? a b))))
        res))
  )

(tests
 "superset?"
  false :=
  (let [res (fresh "res")
        a (fresh-set (range 12))
        b (fresh-set (range 12))]
       (get
        (satisfy
         (and (= a #{1 2 3 4 5 6})
              (= b #{4 5 6 7 8 9})
              (= res (set/superset? a b))))
        res))

  true :=
  (let [res (fresh "res")
        a (fresh-set (range 12))
        b (fresh-set (range 12))]
       (get
        (satisfy
         (and (= a #{1 2 3 4 5 6 7 8 9})
              (= b #{1 2 3})
              (= res (set/superset? a b))))
        res)))