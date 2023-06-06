(ns petrushka.test.terms.core
  (:require [hyperfiddle.rcf :refer [tests]]
            [petrushka.main :as main :refer [?> fresh satisfy]]
            [petrushka.protocols :as protocols]
            [petrushka.types :as types]
            [petrushka.utils.test :refer [throws?]]))

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

(tests "if"
  (tests "validates the test is boolean"
    true := (throws? (?> (if 1 (fresh) (fresh))))
    false := (throws? (?> (if (= 1 (fresh)) (fresh) (fresh))))
    )
  
  (tests "validates the return types are consistent"
    true := (throws? (?> (if (fresh) 1 #{})))
    false := (throws? (?> (if (fresh) #{} #{})))
    )
  
  (tests "evaluates"
    (let [a (fresh)
          b (fresh)]
      0 :=
      (get
       (satisfy
        (and
         (= a 9)
         (if (>= a 10) (= b 1) (= b 0))))
       b)

      1 :=
      (get
       (satisfy
        (and
         (= a 11)
         (if (>= a 10) (= b 1) (= b 0))))
       b)

      10 :=
      (get
       (satisfy
        (and
         (= a 11)
         (= b (+ 5 (if (>= a 10) 5 6)))))
       b))))


