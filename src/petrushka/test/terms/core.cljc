(ns petrushka.test.terms.core
  (:require [hyperfiddle.rcf :refer [tests]]
            [petrushka.api :as api]
            [petrushka.main :as main :refer [?> fresh satisfy]]
            [petrushka.protocols :as protocols]
            [petrushka.types :as types]
            [petrushka.utils.test :refer [throws? only-val]]))

(tests 
 ">="
  (let [a (fresh)
        b (fresh)]
    20 :=
    (get
     (satisfy
      (and
       (= a 20)
       (when (>= a 10)
         (= b 20))))
     b)

    true :=
    (get
     (satisfy
      (and
       (= a 20)
       (when (>= a 10)
         (= b true))))
     b)
    
    
    true := 
    (get
     (satisfy
      (and
       (= a 20)
       (= b (>= 21 a 20 19))))
     b)
    ))

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

(tests "not="

  (not= 1 (only-val (satisfy (not= (fresh) 1))))
  := true

  (not= #{} (only-val (satisfy (not= (api/bind (range 100) (fresh)) #{}))))
  := true)

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

(tests "cond"
  (tests "validates the test is boolean"
    false := (throws? (?> (cond false (fresh) :else (fresh))))
    true := (throws? (?> (cond (= 1 (fresh)) (fresh) (+ 2 3) 4 :else 2)))
    )
  
  (tests "validates the return types are consistent"
    true := (throws? (?> (cond (fresh) 1 (fresh) #{} :else #{})))
    false := (throws? (?> (cond (fresh) #{1 2 3} :else #{})))
    )
  
  (tests "evaluates"
    (let [a (fresh)
          b (fresh)]
      0 :=
      (get
       (satisfy
        (and
         (= a 9)
         (cond (>= a 10) (= b 1) (= a 9) (= b 0) :else false)))
       b)

      1 :=
      (get
       (satisfy
        (and
         (= a 11)
         (cond (>= a 10) (= b 1) (= a 9) (= b 0) :else false)))
       b)

      10 :=
      (get
       (satisfy
        (and
         (= a 11)
         (= b (+ 5 (cond (= a 1) 6 (>= a 10) 5 :else 0)))))
       b))))

(tests "count"
  1 :=
  (count
   (only-val
    (satisfy
     (= 1 (count (api/bind (range 10) (fresh))))))))

