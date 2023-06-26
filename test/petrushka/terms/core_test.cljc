(ns petrushka.terms.core-test
  (:require [hyperfiddle.rcf :refer [tests]]
            [petrushka.main :as main :refer [bind ?> fresh satisfy]]
            [petrushka.protocols :as protocols]
            [petrushka.types :as types]
            [petrushka.utils.test :refer [throws? only-val]]
            [hyperfiddle.rcf :as rcf]
            [petrushka.api :as api]))

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
 "<="
  (let [a (fresh)
        b (fresh)]
    20 :=
    (get
     (satisfy
      (and
       (= a 30)
       (when (<= a 30)
         (= b 20))))
     b)

    true :=
    (get
     (satisfy
      (and
       (= a 20)
       (when (<= a 21)
         (= b true))))
     b)
    
    
    true := 
    (get
     (satisfy
      (and
       (= a 20)
       (= b (<= 19 a 20 21))))
     b)
    ))

(tests 
 ">"
  (let [a (fresh)
        b (fresh)]
    20 :=
    (get
     (satisfy
      (and
       (= a 31)
       (when (> a 30)
         (= b 20))))
     b)

    false :=
    (get
     (satisfy
      (and
       (= a 20)
       (when (> a 21)
         (= b true))))
     b)
    
    
    true := 
    (get
     (satisfy
      (and
       (= a 20)
       (= b (> 21 a 19))))
     b)
    ))

(tests 
 "<"
  (let [a (fresh)
        b (fresh)]
    20 :=
    (get
     (satisfy
      (and
       (= a 20)
       (when (< a 30)
         (= b 20))))
     b)

    true :=
    (get
     (satisfy
      (and
       (= a 20)
       (when (< a 21)
         (= b true))))
     b)
    
    
    true := 
    (get
     (satisfy
      (and
       (= a 20)
       (= b (< 19 a 21))))
     b)
    ))


(tests "zero?"
  0 := (only-val (satisfy (zero? (fresh))))

  42 := (let [a (fresh)
              b (fresh)]
          (->
           (satisfy
            (and (= 0 a)
                 (when (zero? a)
                   (= b 42))))
           (get b))))

(tests "pos?"
  true := (pos? (only-val (satisfy (pos? (fresh)))))

  42 := (let [a (fresh)
              b (fresh)]
          (->
           (satisfy
            (and (= -42 a)
                 (when (not (pos? a))
                   (= b 42))))
           (get b))))

(tests "neg?"
  true := (neg? (only-val (satisfy (neg? (fresh)))))

  42 := (let [a (fresh)
              b (fresh)]
          (->
           (satisfy
            (and (= 42 a)
                 (when (not (neg? a))
                   (= b 42))))
           (get b))))

(tests 
 "+"
 (only-val (satisfy (= (+ 1 (fresh)) 3))) := 2
 )

(tests
 "*"
  (only-val (satisfy (= (* 1 (fresh)) 3))) := 3)

(tests
 "-"
  (only-val (satisfy (= (- 1 (fresh)) 3))) := -2)

(tests
 "dec"
  (only-val (satisfy (= (dec (fresh)) 3))) := 4)

(tests
 "inc"
  (only-val (satisfy (= (inc (fresh)) 3))) := 2)

(tests
 "even?"
  (even? (only-val (satisfy (even? (fresh))))) := true)

(tests
 "odd?"
  (odd? (only-val (satisfy (odd? (fresh))))) := true)

(tests
 "true?"
  (true? (only-val (satisfy (true? (fresh))))) := true)

(tests
 "false?"
  (false? (only-val (satisfy (false? (fresh))))) := true)

(tests
 "max"
  (for [_ (range 5)]
    (let [nums (take 5 (repeatedly (partial rand-int 1000)))]
      (= (apply max nums)
         (only-val (satisfy (= (fresh) (apply max nums)))))
      := true)))

(tests
 "max"
  (for [_ (range 5)]
    (let [nums (take 5 (repeatedly (partial rand-int 1000)))]
      (= (apply min nums)
         (only-val (satisfy (= (fresh) (apply min nums)))))
      := true)))

(tests
 (/ 6 2)
  "/"
  (only-val (satisfy (= (/ 6 (fresh)) 3))) := 2)

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

  (not= #{} (only-val (satisfy (not= (bind (range 100) (fresh)) #{}))))
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
     (= 1 (count (bind (range 10) (fresh))))))))

(tests "mod and rem"
  true :=
  (some?
   (let [n 5]
     (->> (for [x (concat (range (- 0 n) 0) (range 1 (inc n)))
                y (concat (range (- 0 n) 0) (range 1 (inc n)))
                :let [a (fresh)
                      b (fresh)]]
            (?> (and
                 (= a x)
                 (= b y)
                 (= (rem x y) (rem a b))
                 (= (mod x y) (mod a b)))))
          (apply main/conjunction)
          satisfy))))

(tests "let"
  -1 :=
  (tests "let bindings are respected - symbols that would normally be rewritten can be bound."
    (?>
     (let [+ (+ 1 1)]
       (- 1 +)))))

(tests "fn"
  (tests "fn bindings are respected - symbols that would normally be rewritten can be bound."
    2 :=
    (only-val
     (satisfy
      (?>
       (let [plus (fn [> <]
                    (+ > <))]
         (= 3 (plus (fresh) 1))))))))

#_(tests "loop"
  (macroexpand '(?> (loop [+ 0]
                      (if (< + 3)
                        (recur (inc +))
                        +)))) 
  distinct
  cycle
  every?
  some-fn
  (defn hey [a b]
    (+ a b))



  (meta #'hey)
  riddley/walk

  (+ 1 nil))