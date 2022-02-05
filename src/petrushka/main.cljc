(ns petrushka.main
  (:require [hyperfiddle.rcf :refer [tests]]
            [failjure.core :as f]
            [clojure2minizinc.core :as mz]))

(comment
  ;; the representation used for MEDN should be pure, but with extensive caching that is core-async aware.
  ;; it should be able to write back to an open channel when the computation is taking more than x milliseconds.
  ;; the cache is possible via function calls by value -> it is the result of the get that's cached, not the get path 

  ;; this should be a general purpose utility, 
  ;; the goal of which is to send incremental progress updates back to the main thread so it can re-render, at a specified frame-rate
  ;; the computation should be async if neccessary... so that we can implement API backends in a way that's still permormant.
  ;; should be written in such a way that functions that are cached are returned immediatley, while functions that need to request asynchronous resources make their requests in parallel. Let it be up to the processor how they decide to thread it. 

  ;; the other idea, not immediatley applicable here, is that your rautavaara renderers are effectivley just slicing time into a certain control rate... probably 60 hz, or however quickly you can get it. This could be part of the time library. 
  
  )

(defn force-sequence [v]
  (if (sequential? v) v [v]))

(defn extend-cvar-table [cvar [type range :as cvar-meta] table]
  (if-let [[current-type current-range] (get table cvar)]
    (cond
      (and current-type type (not= current-type type))
      (f/fail [:inconsistent-types current-type type])

      (and current-range range (not= (set current-range) (set range)))
      (f/fail [:inconsistent-ranges current-range range])

      :else (assoc table cvar [(or current-type type)
                               (or current-range range)]))
    (assoc table cvar cvar-meta)))

(tests
 (extend-cvar-table :a [:number] {}) := {:a [:number]}

 (:message (extend-cvar-table :a [:number] {:a [:set (range 10)]}))
 := [:inconsistent-types :set :number]

 (extend-cvar-table :a [:number (range 12)] {:a [:number]})
 := {:a [:number (range 12)]}
 )

(defn extend-cvar-table-from-find [{:keys [find]} cvar-table]
  (assert (coll? find))
  (->> find
       (into #{})
       (reduce 
        (fn [acc curr]
          (let [[cvar v] (force-sequence curr)
                {[range] true [type] false} (group-by coll? (force-sequence v))
                new-table (extend-cvar-table cvar [type range] acc)]
            (if (f/failed? new-table)
              (reduced new-table)
              new-table)))
        cvar-table)))

(tests
 (let [example {:find [[:a [(range 0 12) :number]]
                       [:b [:set (range 12)]]
                       :c]
                :where [[:not-in :a (range 12)]
                        [:in :a (range 13)]]
                :solve [:minimize [:+ :a 22]] ;; optional - defaults to satisfy
                :return 3 ;; optional - defaults to 3
                }]
   (extend-cvar-table-from-find example {}) := {:a [:number (range 0 12)]
                             :b [:set (range 0 12)]
                             :c [nil nil]}

   (extend-cvar-table-from-find (assoc example :find {:a [(range 0 12)]
                                      :b [:set (range 0 12)]
                                      :c nil})
                {})
   := {:a [nil (range 0 12)]
       :b [:set (range 0 12)]
       :c [nil nil]}

   (let [duplicate-variables (extend-cvar-table-from-find (update example :find conj [:b :number]) {})]
     (tests
      (f/failed? duplicate-variables) := true
      (:message duplicate-variables) := [:inconsistent-types :number :set]))))

(def ops
  {:+ [:number [2] [:number]] ;; high airity is optional, assumed to be infinite.
   :in [:boolean [2 2] [:number :set]] 
   :not [:boolean [1 1] [:boolean]]
   :and [:boolean [1] [:boolean]]
   := [:boolean [2] [:number]]})

(tests
 (doall
  (for [[_op [_ airity-bounds arg-types]] ops]
    (when (= 2 (count arg-types)) ;; the presence of both a left and right args type implies that it is a binary operator and the airity must be no more or less than 2
      (tests airity-bounds := [2 2])))))

(def cvar? keyword?)

(defn extend-cvar-table-from-operator-expression [cvar-table expression]
  (let [[_ [low-airity high-airity] [left-type right-type :as arg-types]] (get ops (first expression))
        extend-cvar-table-from-arg (fn [cvar-table arg-type-pair]
                                     (if (f/failed? arg-type-pair)
                                       (reduced arg-type-pair)
                                       (let [[arg type] arg-type-pair
                                             known-type-of-arg (cond
                                                                 (cvar? arg) (get-in cvar-table [arg 0])
                                                                 (vector? arg) (get-in ops [(first arg) 0])
                                                                 (number? arg) :number
                                                                 (set? arg) :set)]
                                         (cond
                                           (and known-type-of-arg (not= known-type-of-arg type))
                                           (f/fail [:inconsistent-types (str "type of " arg " bound to " known-type-of-arg
                                                                             " but used as " type " in expression " expression)])

                                           (cvar? arg)
                                           (extend-cvar-table arg [type] cvar-table)

                                           :else cvar-table))))]
    ;; todo airity checking
    (->> (interleave (rest expression) (if right-type arg-types (repeat left-type)))
         (partition 2)
         (reduce extend-cvar-table-from-arg cvar-table))))

(tests
 (extend-cvar-table-from-operator-expression {} [:+ :a :b]) := {:a [:number], :b [:number]}
 (f/failed? (extend-cvar-table-from-operator-expression {:a [:set]} [:+ :a :b])) := true
 (f/failed? (extend-cvar-table-from-operator-expression {:a [:set]} [:in :b :a])) := false
 )

(defn extend-cvar-table-from-constraint [cvar-table constraint]
  (cond
    (f/failed? cvar-table) cvar-table

    (vector? constraint)
    (extend-cvar-table-from-operator-expression 
     (reduce extend-cvar-table-from-constraint cvar-table constraint)
     constraint)

    :else cvar-table))

(tests 
 (extend-cvar-table-from-constraint {} [:in :a :b]) := {:a [:number], :b [:set]}

 (f/failed? (extend-cvar-table-from-constraint {} [:in [:+ :a :b] :b])) := true

 (extend-cvar-table-from-constraint {:a [:number]} [:and 
                                                    [:= [:+ 1 :a] 2] 
                                                    [:in :a :b]])
 := {:a [:number nil], :b [:set]}

 (f/failed? (extend-cvar-table-from-constraint {:a [:number]} [:and
                                                               [:in [:+ [:+ 1 10] :c] :d]
                                                               [:= [:+ 1 :a] 2]
                                                               [:in :a :b]]))
 := false

 (f/failed? (extend-cvar-table-from-constraint {:a [:number]} [:and
                                                               [:in [:+ [:+ 1 10] :b] :d]
                                                               [:= [:+ 1 :a] 2]
                                                               [:in :a :b]]))
 := true)

(defn interpret [{:keys [where find]}]
  (f/ok->> {}
           (extend-cvar-table-from-find find)
           (extend-cvar-table-from-where where)
           ;; validate that everything has an associated type and range, if required
           ;; translate to minizinc, using the lvar table for var declarations and the where clause for constraints 
           ;; send over the wire.
           )

  #_(f/attempt-all [cvars-extended-from-find (extend-cvar-table-from-find find {})
                  cvars-extended-from-where (extend-cvar-table-from-where where cvars-extended-from-find)]))

(comment
  
  (extend-cvar-table-from-constraint {} [:a [:b 1 2 [:= 1 2]]])

  (clojure.walk/postwalk-demo [[:and 1 2] [:and 1 2 4]])
  
  )

(tests
 (let [example {:find [[:a [(range 0 20) :number]]
                       [:b [:set (range 12)]]
                       :c]
                :where [[:not-in :a (range 12)]
                        [:in :a (range 13)]]
                :solve [:minimize [:+ :a 22]] ;; optional - defaults to satisfy
                :return 3 ;; optional - defaults to 3
                }]
   
   ))

(comment
  
  (hyperfiddle.rcf/enable!)

  
  ;; let's translate our first minizinc model into something that you'd like to be able to call from clojure.

;; set of int: index_set = 1..4;
;; hopefully, this won't be needed or could be infered

;; array[index_set] of set of 0..11: input = [{0, 2, 4, 8}, {1, 3, 4, 7, 9, 10}, {0}, {2, 3, 5}];
  ;; array type declaration should be automatic if possible. 

;; array[index_set] of var set of 0..11: results;

;; % constraint forall(c in results[1])( ((c + 1) mod 12) in results[1] -> not ( ((c + 2) mod 12) in results[1] ) );


;; predicate cluster_free(var set of 0..11: s) = forall(c in s)( ((c + 1) mod 12) in s -> not ( ((c + 2) mod 12) in s ) );

;; function var int: overlap(var set of 0..11: a, var set of 0..11: b) = card(a intersect b);

;; constraint forall(r in results)(cluster_free(r));

;; var int: parsimony = sum(r in index_set where r <= card(index_set) - 1 )( overlap(results[r], results[r + 1]) );

;; %superset constraint... should always hold
;; constraint forall(r in index_set)(results[r] superset input[r]);

;; % cardinality
;; constraint forall(r in index_set)(card(results[r]) <= 6);


;; % fifth drone constraint
;; constraint forall(r in index_set)({0} subset results[r] \/ {7} subset results[r]);

;; solve maximize parsimony;

;; output(["\(parsimony), \(results)"]);

  )

(defn plus [x y] (+ x y))

(comment
  (plus 2 3)




  ;; what don't you like about clj to minizinc? 

  ;; 1 doesn't feel natural to clojure programming. Too much reliance on clj to minizinc functions. Feels basically like a small clojure wrapper around minizinc. 
  ;; macro-based syntax is hard to extend or understand... ideally this will be a relativevly pure function of clojure to minizinc code.
  ;; too many manual wranging of minizinc types. We should be able to infer types.
  ;; no native support of basic clojure data structures.... I want to be able to make sets!
  ;; one limitation of this will be that there will be a more restricted semantic for sets as well as map keys and values.
  ;; will thar be???

  ;;In MiniZinc decision variables can have the following types: Booleans, integers, floats, and sets of integers, and enums. Arrays and ann can contain decision variables.

  ;; the most important thing here, maybe the only important thing, is integer and set optimization. Sets, arrays of sets, and integers are the main things to work on now.  

  {:find [[:a (range 0 12)]
          [:b :set (range 12)]
          [:b true (range 12)]
          :c ;; can a type be infered here in many cases? is declaration of a set neccessary?
          ;; let's default to an int, cast to a set when required by our simple type checker
          ;; is it safe to assume that we don't have to collect integer type hints?
          ]
   :where [[:not-in :a (range 12)]
           [:in :a (range 13)]]
   :solve [:minimize [:+ :a 22]] ;; optional - defaults to satisfy
   :return 3 ;; optional - defaults to 3
   }

  (keyword (str 1))

  (let [vars (map (comp keyword str) (range 10))]
    {:find (for [v vars]
             [v :set (range 12)])
     :where (concat
             (for [[x y] (partition 2 1 (range 10))]
               [:> 2 [:card [:intersection x y]]])
             [:+ (for [v vars]
                   [:some-func v])])})

  )