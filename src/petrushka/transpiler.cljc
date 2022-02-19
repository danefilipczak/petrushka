(ns petrushka.transpiler
  (:require
   [petrushka.utils.string :refer [>>]]
   [failjure.core :as f]
   [clojure.string :as string]
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
                            (>>* "var int: {{cvar}};"))
                  :boolean (>>* "var bool: {{cvar}};")
                  cvar))))
       sort))

(tests
 (cvar-table->string-seq {:a [:set #{0 1 2 3 4 5 6 7 8 9 10 11}], :b [:number]})
 := ["var int: b;" "var set of {0,7,1,4,6,3,2,11,9,5,10,8}: a;"]

 (cvar-table->string-seq {:a [:boolean], :b [:number]})
 := ["var bool: a;" "var int: b;"])

(defn apply-binary-operation [op-string left right]
  (>> {:left left :right right :op-string op-string}
      "({{left}}{{op-string}}{{right}})"))

(tests
 (apply-binary-operation "+" 1 2) := "(1+2)")

(defn apply-nary-fold [op-string args]
  (reduce (partial apply-binary-operation op-string) args))

(tests
 (apply-nary-fold "+" [1 2 3]) := "((1+2)+3)"
 (apply-nary-fold "+" [1]) := 1)

(defn as-array [string-exs]
  (>> {:elements (apply str (interpose "," string-exs))}
      "[{{elements}}]"))

(tests
 (as-array ["1" "2"]) := "[1,2]"
 (as-array ["1"]) := "[1]")

(defn xall [call-string string-exs]
  (>> {:element-array (as-array string-exs)
       :call-string call-string}
      "{{call-string}}({{element-array}})"))

(defn forall [string-exs]
  (xall "forall" string-exs))

(defn xorall [string-exs]
  (xall "xorall" string-exs))

(defn iffall [string-exs]
  (xall "iffall" string-exs))

(tests
 (iffall ["true" "false"])
 (forall ["1+2=3" "3=4"]) := "forall([1+2=3,3=4])")

(defn apply-nary-comparison [op-string args]
  (->> args
       (partition 2 1)
       (map (partial apply apply-binary-operation op-string))
       (forall)))

(tests
 (apply-nary-comparison ">" [1 2 3 4 5]) := "forall([(1>2),(2>3),(3>4),(4>5)])"
 (apply-nary-comparison ">" [1 2]) := "forall([(1>2)])")

(defmulti op-expression->string (fn [expression] (first expression)))

(defn constraint-expression->string [constraint-expression]
  (clojure.walk/postwalk
   (fn [e]
     (cond
       (vector? e) (op-expression->string e)
       (cvar? e) (cvar->string e)
       (or (sequential? e) (set? e)) (seq->string e)
       :else e))
   constraint-expression))

(tests
 (constraint-expression->string [:+ [:+ 1 2] [:- 3 2] [:+ 3 4]]) :=  "(((1+2)+(3-2))+(3+4))")

(defmethod op-expression->string :+
  [ex]
  (apply-nary-fold "+" (rest ex)))

(defmethod op-expression->string :-
  [ex]
  (apply-nary-fold "-" (rest ex)))

(defmethod op-expression->string :and
  [[_ & args]]
  (cond 
    (= 1 (count args)) (first args)
    (= 2 (count args)) (apply apply-binary-operation "/\\" args)
    :else (forall args)))

(tests
 (op-expression->string [:and true true false]) := "forall([true,true,false])"
 (op-expression->string [:and true true]) := "(true/\\true)"
 (op-expression->string [:and false]) := false
 )

(defmethod op-expression->string :or
  [[_ & args]]
  (cond
    (= 1 (count args)) (first args)
    (= 2 (count args)) (apply apply-binary-operation "\\/" args)
    :else (apply-nary-fold "\\/" args)))

(tests
 (op-expression->string [:or true true false]) := "((true\\/true)\\/false)"
 (op-expression->string [:or true true]) := "(true\\/true)"
 (op-expression->string [:or false]) := false)

(defmethod op-expression->string :xor
  [[_ & args]]
  (cond
    (= 1 (count args)) (first args)
    (= 2 (count args)) (apply apply-binary-operation " xor " args)
    :else (xorall args)))

(tests
 (op-expression->string [:xor true true false]) := "xorall([true,true,false])"
 (op-expression->string [:xor true true]) := "(true xor true)"
 (op-expression->string [:xor false]) := false)

(defmethod op-expression->string :iff
  [[_ & args]]
  (cond
    (= 1 (count args)) (first args)
    (= 2 (count args)) (apply apply-binary-operation "<->" args)
    :else (iffall args)))

(defmethod op-expression->string :when
  [[_ & args]]
  (apply apply-binary-operation "->" args))

(defn apply-comparator [[op & args]]
  (cond 
    (= 1 (count args)) true
    (= 2 (count args)) (apply apply-binary-operation (name op) args)
    :else (apply-nary-comparison (name op) args)))

(defmethod op-expression->string := [ex] (apply-comparator ex))

(defmethod op-expression->string :set= [ex]
  (constraint-expression->string (concat [:=] (rest ex))))

(defmethod op-expression->string :> [ex] (apply-comparator ex))

(defmethod op-expression->string :< [ex] (apply-comparator ex))

(defmethod op-expression->string :<= [ex] (apply-comparator ex))

(defmethod op-expression->string :>= [ex] (apply-comparator ex))

(defmethod op-expression->string :contains? [ex] (apply apply-binary-operation " in " (->> ex rest reverse)))

(tests
 (op-expression->string [:< 1 2 3]) := "forall([(1<2),(2<3)])")

(defmethod op-expression->string :not [ex]
  (>> {:ex (second ex)} "(not {{ex}})"))

(defmethod op-expression->string :if
  [ex]
  (let [[_ if then else] ex]
    (constraint-expression->string
     [:or
      [:and if then]
      [:and [:not if] else]])))

(defmethod op-expression->string :true?
  [ex]
  (constraint-expression->string [:= (rest ex) true]))

(defmethod op-expression->string :false?
  [ex]
  (constraint-expression->string [:= (rest ex) false]))

(tests
 (op-expression->string [:if :test :clause :else]) := "((test/\\clause)\\/((not test)/\\else))"
 (op-expression->string [:if [:= 1 1] [:> 55 :a] [:< 55 :b]]) := "(((1=1)/\\(55>a))\\/((not (1=1))/\\(55<b)))"
 )

(tests
 ;; test all operators have implementations
 (clojure.set/difference 
  (set (keys ops/all))
  (set (keys (methods op-expression->string)))) := #{})

(defn where->constraints [where]
  (map (fn [ex] 
         (>> {:expression (constraint-expression->string ex)}
             "constraint {{expression}};")) 
       where))

;;output(["[\"\(a)\" \"\(b)\"]"])
(defn ->output [cvar-table]
  (let [var-string (->> (for [cvar (-> cvar-table keys sort)]
                          (>> {:var-string (cvar->string cvar)}
                              "\\\"\\({{var-string}})\\\""))
                        (interpose " ")
                        (apply str))]
    (>> {:vars var-string}
        "output([\"[{{vars}}]\"]);")))

(tests
 (->output {:a 1 :b 2}) := "output([\"[\\\"\\(a)\\\" \\\"\\(b)\\\"]\"]);")

(defn transpile [{:keys [where] :as _query} cvar-table]
  (let [var-declarations (cvar-table->string-seq cvar-table)
        constraints (where->constraints where)
        output (->output cvar-table)]
    (apply str (interpose "\n" (conj (concat var-declarations constraints) output)))))

(defmulti detranspile*
  (fn [cvar-table [cvar out-str]]
    (get-in cvar-table [cvar 0])))

(defmethod detranspile* :number [_ [_ out-str]]
  (Integer/parseInt out-str))

(defmethod detranspile* :boolean [_ [_ out-str]]
  (Boolean/parseBoolean out-str))

(defmethod detranspile* :set [_ [_ out-str]]
  (if (re-matches #"[0-9]*\.\.[0-9]*" out-str)
    (let [[lower upper] (->> (string/split out-str #"\.\.")
                             (map #(Integer/parseInt %)))]
      (apply sorted-set (range lower (+ 1 upper))))
    (read-string (str "#" out-str))))

(defn detranspile [& [out-str cvar-table :as args]]
  (def margs args)
  (->> (string/split out-str #"\n")
       first
       read-string
       (interleave (-> cvar-table keys sort))
       (partition 2)
       (map (partial detranspile* cvar-table))
       (zipmap (-> cvar-table keys sort))))

(tests
 (detranspile
  "[\"0..11\" \"3\"]\n----------\n"
  {:a [:set #{0 1 2 3 4 5 6 7 8 9 10 11}], :b [:number]}) 
 := {:a #{0 1 2 3 4 5 6 7 8 9 10 11}, :b 3}

 (detranspile 
  "[\"{0,2,3,4,5,6,7,8,9,10,11}\" \"3\"]\n----------\n"
  {:a [:set #{0 2 3 4 5 6 7 8 9 10 11}], :b [:number]}) 
 := {:a #{0 7 4 6 3 2 11 9 5 10 8}, :b 3}
 )

(comment
  

  (spit "test-output"
        (transpile
         {:where [[:contains? :a :b]
                  [:> [:+ :b 6] 8]]}
         {:a [:set #{0 2 3 4 5 6 7 8 9 10 11}], :b [:number]}))

  (def st )

  )