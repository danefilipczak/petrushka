(ns petrushka.analyzer
  "Performs type inference and airity checking while constructing a map of constraint variables
   with their associated types and ranges."
  (:require [hyperfiddle.rcf :refer [tests]]
            [failjure.core :as f]
            [petrushka.utils.cvar :refer [cvar?]]
            [petrushka.operations :as ops]))

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
 (:message (extend-cvar-table :a [:number] {:a [:set (range 10)]})) := [:inconsistent-types :set :number]
 (extend-cvar-table :a [:number (range 12)] {:a [:number]}) := {:a [:number (range 12)]}
 )

(defn extend-cvar-table-from-find [find cvar-table]
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
 (extend-cvar-table-from-find [[:a [(range 0 12) :number]]
                               [:b [:set (range 12)]]
                               :c] {})
 := {:a [:number (range 0 12)]
     :b [:set (range 0 12)]
     :c [nil nil]}

 (extend-cvar-table-from-find
  {:a [(range 0 12)]
   :b [:set (range 0 12)]
   :c nil}
  {})
 := {:a [nil (range 0 12)]
     :b [:set (range 0 12)]
     :c [nil nil]}

 (let [duplicate-variables (extend-cvar-table-from-find [[:a [(range 0 12) :number]]
                                                         [:b [:set (range 12)]]
                                                         :c
                                                         [:b :number]] {})]
   (tests
    (f/failed? duplicate-variables) := true
    (:message duplicate-variables) := [:inconsistent-types :number :set])))

(defn type-of-expression [expression cvar-table]
  (cond
    (cvar? expression) (get-in cvar-table [expression 0])
    (vector? expression) (get-in ops/all [(first expression) 0]) ;; this could recur here when the expression is of type any
    (number? expression) :number
    (set? expression) :set
    (boolean? expression) :boolean))

(defn extend-cvar-table-from-operator-expression [cvar-table expression]
  (let [[_ [low-airity high-airity] [left-type right-type :as arg-types]] (get ops/all (first expression))
        extend-cvar-table-from-arg (fn [cvar-table arg-type-pair]
                                     (if (f/failed? arg-type-pair)
                                       (reduced arg-type-pair)
                                       (let [[arg operator-type] arg-type-pair
                                             known-type-of-arg (type-of-expression arg cvar-table)]
                                         (cond
                                           (= operator-type :any) (if (cvar? arg)
                                                                    (extend-cvar-table arg nil cvar-table)
                                                                    cvar-table)

                                           (and known-type-of-arg
                                                operator-type
                                                (not= known-type-of-arg operator-type))
                                           (f/fail [:inconsistent-types (str "type of " arg " bound to " known-type-of-arg
                                                                             " but used as " operator-type " in expression " expression)])

                                           (cvar? arg)
                                           (extend-cvar-table arg [operator-type] cvar-table)

                                           :else cvar-table))))]
    ;; todo airity checking
    (->> (interleave (rest expression) (if right-type
                                         (conj (repeat right-type) left-type)
                                         (repeat left-type)))
         (partition 2)
         (reduce extend-cvar-table-from-arg cvar-table))))

(tests
 (extend-cvar-table-from-operator-expression {} [:if :a :b :c]) := {:a [:boolean], :b [:boolean], :c [:boolean]}
 (f/failed? (extend-cvar-table-from-operator-expression {} [:xor [:if :a [:contains? :s 1] false] [:contains? :s :b]])) := false
 (extend-cvar-table-from-operator-expression {} [:if :a [:> 1 2]]) := {:a [:boolean]}
 (extend-cvar-table-from-operator-expression {} [:if :a :c false]) := {:a [:boolean], :c [:boolean]}
 (extend-cvar-table-from-operator-expression {} [:+ :a :b]) := {:a [:number], :b [:number]}
 (f/failed? (extend-cvar-table-from-operator-expression {:a [:set]} [:+ :a :b])) := true
 (f/failed? (extend-cvar-table-from-operator-expression {:a [:set]} [:contains? :a :b])) := false
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
 (f/failed? (extend-cvar-table-from-constraint {} [:if [:not :a] [:contains? :b :a] [:contains? :c :a]])) := true
 (f/failed? (extend-cvar-table-from-constraint {} [:if [:not :a] [:contains? :b :d] [:contains? :c :b]])) := true
 (f/failed? (extend-cvar-table-from-constraint {} [:if [:not :a] [:contains? :b :d] [:contains? :c :d]])) := false

 (extend-cvar-table-from-constraint {} [:contains? :b :a]) := {:a [:number], :b [:set]}
 (f/failed? (extend-cvar-table-from-constraint {} [:contains? :b [:+ :a :b]])) := true
 (extend-cvar-table-from-constraint {:a [:number]} [:and
                                                    [:= [:+ 1 :a] 2]
                                                    [:contains? :b :a]])
 := {:a [:number nil], :b [:set]}

 (f/failed?
  (extend-cvar-table-from-constraint
   {:a [:number]}
   [:and
    [:contains? :d [:+ [:+ 1 10] :c]]
    [:= [:+ 1 :a] 2]
    [:contains? :b :a]]))
 := false

 (f/failed?
  (extend-cvar-table-from-constraint
   {:a [:number]}
   [:and
    [:contains? :d [:+ [:+ 1 10] :b]] ;; :b used as a number
    [:= [:+ 1 :a] 2]
    [:contains? :b :a]]));; :b used as a set
 := true)

(defn extend-cvar-table-from-where [where cvar-table]
  (reduce
   (fn [cvar-table constraint]
     (if (f/failed? constraint)
       (reduced constraint)
       (if-not (= :boolean (type-of-expression constraint cvar-table))
         (f/fail [:malformed-constraint "constraints must be boolean expressions"])
         (extend-cvar-table-from-constraint cvar-table constraint))))
   cvar-table
   where))

(defn ->cvar-table [{:keys [where find] :as query}]
  (f/ok->> {}
           (extend-cvar-table-from-find find)
           (extend-cvar-table-from-where where)
           ;; TODO validate that everything has an associated type and range, if required
           ;; TODO airity checking
           ))
