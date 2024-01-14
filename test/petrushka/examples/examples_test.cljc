(ns petrushka.examples.examples-test
  (:require [hyperfiddle.rcf :refer [tests]]
            [petrushka.protocols :as protocols]
            [petrushka.main :as main :refer [conjunction bind ?> fresh satisfy solve-for]]
            [petrushka.types :as types]
            [petrushka.solver :as solver]
            [petrushka.utils.test :refer [throws?]]
            [hyperfiddle.rcf :as rcf]))


(tests 
 (let [mesos (take 5 (repeatedly fresh))
       cluster-free (fn [set-decision]
                      (?>
                       (main/forall [a (bind (range 12) set-decision)]
                         (when (contains? set-decision (mod (+ a 1) 12))
                           (not (contains? set-decision (mod (+ a 2) 12)))))))
       constraint (?>
                   (apply
                    conjunction
                    (concat
                     (->> mesos
                          (partition 2 1)
                          (map
                           (fn [[a b]]
                             (and
                              (not= a b)
                              (= (count (clojure.set/intersection a b)) 3)))))
                     (map cluster-free mesos)
                     (map (comp (partial = 4) count) mesos))))
       solution (satisfy constraint)
       mesos* (map solution mesos)]
   true := 
   (and (every?
         true?
         (map
          (fn [x]
            (= 4 (count x)))
          mesos*))
        (every?
         true?
         (map
          (fn [[a b]]
            (and
             (not= a b)
             (= 3 (count (clojure.set/intersection a b)))))
          (partition 2 1 mesos*)))))

  )

(defn pitchclass [x] 
  (?> (contains? (into #{} (range 12)) x)))

(defn interval-class [i]
  (?>
   (let [abs-i (if (< i 0)
                 (- i)
                 i)
         semitones (mod abs-i 12)]
     (if (> semitones 6)
       (- 12 semitones)
       semitones))))

(def interval-necklace-pcs
  ;; one idea is to rewrite this using a disjunction of a chromatic range rather than a separate int decision.
  ;; that would likely work, but the deeper reason this isn't succeeding is that we need to flatten our conjunctions
  (fn [pitch-class-set intervals]
    (let [n (fresh)]
      (?> (and
           (= (count (bind (range 12) pitch-class-set)) (count intervals))
           (pitchclass n) 
           (apply conjunction
                  (for [i (range (count intervals))]
                    (contains?
                     pitch-class-set
                     (mod (apply + n (take (inc i) intervals)) 12)))))))))

(comment

  (defn necklace [& args]
    ;; todo write in such a way that it succeeds transparently when passed ints
    ;; this can be done by using a disjunction and iterating over a range rather than introducing a new int var
    (?>
     (let [i (fresh)]
       (apply
        conjunction
        (pitchclass i)
        (apply < (map (comp #(mod % 12) (partial + i)) args))
        (map pitchclass args)))))

  (def interval-necklace
    (fn [pitch-classes intervals]
      (let [n (fresh)]
        (assert (= (count pitch-classes) (count intervals)))
        (?> (and
             (pitchclass n)
             (apply necklace pitch-classes)
             (apply conjunction (map pitchclass pitch-classes))
             (apply conjunction
                    (for [[i [a b]] (zipmap intervals (partition 2 1 (concat pitch-classes [(first pitch-classes)])))]
                      (=
                       (mod (- b a) 12)
                       (mod (+ i n) 12)))))))))


  (let [pcs (take 3 (repeatedly fresh))
        solution (satisfy
                  (and
                   (= (first pcs) 10)
                   (interval-necklace
                    pcs
                    [4 3 5])))]
    (map (fn [x] (get solution x)) pcs))

  (let [chords (take 5 (repeatedly fresh))
        first-is-d-minor (?> (= (first chords) #{2 5 9}))
        last-is-eb-minor (?> (= (last chords) #{3 6 10}))
        cards (?> (apply conjunction (map #(= (count (bind (range 12) %)) 3) chords)))
        parsimony (?> (apply
                       conjunction
                       (for [[a b] (partition 2 1 chords)]
                         (= (count (clojure.set/intersection a b)) 2))))
        structure (?> (apply
                       conjunction
                       (for [c chords]
                         (or (interval-necklace-pcs c [3 4 5])
                             (interval-necklace-pcs c [4 3 5])))))
        solution (satisfy (and
                           structure
                           #_(interval-necklace-pcs (first chords) [3 4 5])
                           #_cards
                           first-is-d-minor
                           #_last-is-eb-minor
                           #_parsimony))]
    (map #(get solution %) chords))

  (satisfy
   (let [x (fresh)]
     (and
      (= x #{0 4 7})
      (interval-necklace-pcs x [4 3 5]))))
  ;; it proves this 'right' quickly
  

  (let [chords (take 5 (repeatedly fresh))
        structure (?> (apply
                       conjunction
                       (for [c chords]
                         (or (interval-necklace-pcs c [3 4 5])
                             (interval-necklace-pcs c [4 3 5])))))
        solution (satisfy (and
                           structure
                           (?> (= (first chords) #{2 5 9}))
                           #_(= x #{0 4 7})
                           #_(interval-necklace-pcs x [4 3 5])))]
    (map #(get solution %) chords))

  (binding [solver/*debug* true]
    (satisfy
     (?>
      (let [x (fresh)]
        (and (?> (= x #{2 5 9}))
             (or (interval-necklace-pcs x [3 4 5])
                 #_(interval-necklace-pcs x [4 3 5]))))))) 
  
  (binding [solver/*debug* true]
    (satisfy
     (?>
      (let [x (main/fresh-set (range 12))]
        (and (> 2 (fresh)) 
             (and (?> (= x (clojure.set/intersection #{2 5 9} (main/fresh-set (range 12)))))
                  (or (= x #{2 5 9})
                      (= x #{2 3 9}))))))))
  
  (binding [solver/*debug* true]
    (satisfy
     (?>
      (let [x (main/fresh-set (range 12))]
        (= x (clojure.set/intersection #{2 5 9} 
                                       (main/fresh-set (range 12))))))))

  (binding [solver/*debug* true]
    (let [x (fresh)
          solution (satisfy
                    (?>
                     (and (= x #{2 5 9})
                          (or #_(interval-necklace-pcs x [3 4 5])
                              (interval-necklace-pcs x [4 3 5])))))] ;; pretty close - uncommenting throws an err. getting there!
      (get solution x))) 
  
  (binding [solver/*debug* true]
    (let [one (fresh "necklace1")
          two (fresh "necklace2")
          x #{0 4 7}]
      (satisfy (and
                (and (= x one) (= x two)) 
                (interval-necklace-pcs one [3 4 5])
                (interval-necklace-pcs two [4 3 5]))))) 
  
  (binding [solver/*debug* true]
    (let [x (fresh)
          solution (satisfy (interval-necklace-pcs x [3 4 5]))]
      (get solution x))) 
  
  ;; observing the generated minizinc from above, we see that the flattening fails at the introduction of the binding. 
  ;; This is because the 'bound' term is not equal to the var in the ast. 
  ;; we need to make bindings referentially transparent by switching to metadata. 
  
  (satisfy
   (let [x (fresh)]
     (and
      (= x #{0 3 7})
      (interval-necklace-pcs x [4 3 5]))))
  ;; it's unable to prove this 'wrong' in a reasonable amount of time. why?
  ;; it couldn't prove it wrong because of the ol 'pitchclass only registering >0 and not less than 11' bug. 
  ;; it is fixed after loading api. please track down and fix this. 
  
  (binding [api/*debug* true]
    (satisfy
     (let [x (fresh)]
       (and
        (= x #{1 8})
        (interval-necklace-pcs x [8 4])))))


  )