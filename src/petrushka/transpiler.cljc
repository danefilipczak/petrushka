(ns petrushka.transpiler
  (:require 
   [petrushka.utils.string :refer [>>]]
   [failjure.core :as f]
   [hyperfiddle.rcf :refer [tests]]
   [petrushka.utils.cvar :refer [cvar?]]
   [petrushka.operations :as ops]))

(defn cvar->string [cvar]
  (name cvar))

(defn seq->string [s]
  (>> {:elements (apply str (interpose "," s))} "{{{elements}}}"))

(tests
 (seq->string (range 12)) := "{0,1,2,3,4,5,6,7,8,9,10,11}")

(defn cvar-table->string-seq [cvar-table]
  (->> cvar-table
       (map (fn [[cvar [type range]]]
              (let [env {:range (seq->string range)
                         :cvar (cvar->string cvar)}
                    >>* (partial >> env)]
                (case type
                  :set (>>* "var set of {{range}}: {{cvar}};")
                  :number (if range
                            (>>* "var {{range}}: {{cvar}};")
                            (>>* "var float: {{cvar}};"))
                  :boolean (>>* "var bool: {{cvar}};")
                  cvar))))
       sort))

(tests
 (cvar-table->string-seq {:a [:set #{0 1 2 3 4 5 6 7 8 9 10 11}], :b [:number]})
 := ["var float: b;" "var set of {0,7,1,4,6,3,2,11,9,5,10,8}: a;"]
 
 (cvar-table->string-seq {:a [:boolean], :b [:number]}) 
 := ["var bool: a;" "var float: b;"]
 )

(defn apply-binary-operation [op-string left right]
  (>> {:left left :right right :op-string op-string}
      "({{left}}{{op-string}}{{right}})"))

(tests
 (apply-binary-operation "+" 1 2) := "(1+2)")

(defn apply-nary-operation [op-string args]
  (reduce (partial apply-binary-operation op-string) args))

(tests
 (apply-nary-operation "+" [1 2 3]) := "((1+2)+3)")

(defmulti op-expression->string (fn [expression] (first expression)))

(defmethod op-expression->string :+
  [ex]
  (apply-nary-operation "+" (rest ex)))

(defmethod op-expression->string :-
  [ex]
  (apply-nary-operation "-" (rest ex)))

(comment

  (clojure.walk/postwalk
   (fn [e]
     (cond
       (vector? e) (op-expression->string e)
       (cvar? e) (cvar->string e)
       (or (sequential? e) (set? e)) (seq->string e)
       :else e))
   [:+ 1 2 [:- 3 2] [:+ 3 4]])

  (op-expression->string [:- 1 2 3])
  )

(tests
 (clojure.set/difference
  (set (keys ops/all))
  (set (keys (methods op-expression->string)))) := #{})