(ns petrushka.api
  (:require [clojure.java.shell :as shell]
            [clojure.spec.alpha :as spec]
            [clojure.string :as string]
            [hyperfiddle.rcf :refer [tests]]
            [petrushka.utils.string :refer [>>]]
            [petrushka.adapter :as adapter]
            [petrushka.types :as types]
            [petrushka.protocols :as protocols]
            [petrushka.utils.test :as utils.test]
            [clojure.spec.alpha :as s]))

(def ^:dynamic *debug* false)

(defn type-error! [expression v1 v2]
  (let [[type1 exp1] (first v1)
        [type2 exp2] (first v2)
        jared (fn [e type]
                (str
                 (if (= e expression)
                   "is of type "
                   "is used as ")
                 type
                 (when-not (= e expression)
                   (str " in expression " (protocols/write e)))))]
    (throw (ex-info
            (str "inconsistent model: expression "
                 (protocols/write expression)
                 " "
                 (jared exp1 type1)
                 " but "
                 (jared exp2 type2))
            {}))))

(defn map-intersection [m1 m2]
  (select-keys m2 (keys m1)))

(defn intersect-domains
  "given an expression and two domains,
   return the intersection of those domains 
   or throw if their intersection is empty."
  [expression domain1 domain2]
  {:pre [(and (spec/valid? ::domain domain1)
              (spec/valid? ::domain domain2))]}
  (if-let [intersection (not-empty (map-intersection domain1 domain2))]
    intersection
    (type-error! expression domain1 domain2)))

(tests
 (intersect-domains 2 {types/Bool 2} {types/Bool 2}) := {types/Bool 2}
 (intersect-domains 2 {types/Bool 2} {types/Bool 2 types/Numeric 2}) := {types/Bool 2}
 (intersect-domains 2 {types/Bool 2} {types/Bool 3}) := {types/Bool 3} ;; right to left precidence for conflicting keys
 (utils.test/throws? (intersect-domains 2 {types/Bool 2} {types/Numeric 2})) := true
 )

(defn binding-set [x]
  {:pre [(s/valid? (spec/nilable ::binding) x)]}
  (first x))

(defn binding-source [x]
  {:pre [(s/valid? (spec/nilable ::binding) x)]}
  (second x))

(defn binding-error! [decision binding1 binding2]
  (throw (ex-info
            (str "inconsistent model:  "
                 (protocols/write decision)
                 " is bound to non-intersecting sets "
                 (protocols/write (binding-set binding1))
                 " via "
                 (protocols/write (binding-source binding1))
                 " and "
                 (protocols/write (binding-set binding2))
                 " via "
                 (protocols/write (binding-source binding2)))
            {})))

(defrecord Decision [id]
  protocols/IExpress
  (protocols/write [self] (list 'fresh (:id self)))
  (codomain [self] (zipmap types/all-decision-types (repeat self)))
  (decisions [self] {self (zipmap types/all-decision-types (repeat self))})
  (bindings [self] nil)
  (validate [self] self)
  (translate [self] (str (:id self))))

(defn fresh
  "Mint a fresh decision."
  ([]
   (fresh (str (gensym))))
  ([id]
   {:pre [(string? id)]}
   (if (re-matches #"[A-Za-z][A-Za-z0-9_]*" id)
     (->Decision id)
     (throw (ex-info
             (>> {:id id}
                 "Invalid identifier: {{id}}. Identifiers should start with a letter and consist only of letters, numbers, and underscores.")
             {})))))

(defrecord Binding [set decision]
  protocols/IExpress
  (write [self] (list 'bind set (protocols/write decision)))
  (codomain [self] (protocols/codomain decision))
  (decisions [self] (protocols/decisions decision))
  (validate [_self] (protocols/validate decision))
  (translate [_self] (protocols/translate decision))
  (bindings [self] {decision [set self]}))

(def decision? (partial instance? Decision))
(def binding? (partial instance? Binding))
(def decidable? (some-fn decision? binding?))

(defn decidable->decision [x]
  {:pre [(decidable? x)]
   :post [(decision? %)]}
  (if (binding? x)
    (:decision x)
    x))

(defn bind [set decision]
  {:pre [(decision? decision)]}
  (->Binding (apply sorted-set set) decision))

(defn intersect-bindings
  "given a decision and two bindings,
   return the intersection of those bindings 
   or throw if their intersection is empty."
  [expression decision binding1 binding2]
  {:pre [(and (spec/valid? ::binding binding1)
              (spec/valid? ::binding binding2))]
   :post [(spec/valid? ::binding %)]}
  (if-let [intersection (not-empty 
                         (clojure.set/intersection 
                          (first binding1) 
                          (first binding2)))]
    [intersection expression]
    (binding-error! decision binding1 binding2)))

(tests
 (let [a (fresh)]
   (intersect-bindings
    :exp
    a
    (get (protocols/bindings (bind (range 0 10) a)) a)
    (get (protocols/bindings (bind (range 0 5) a)) a))
   := [#{0 1 2 3 4} :exp]

   (intersect-bindings
    :exp
    a
    (get (protocols/bindings (bind (range 0 10) a)) a)
    (get (protocols/bindings (bind (range 0 10) a)) a))
   := [#{0 1 2 3 4 5 6 7 8 9} :exp]

   (utils.test/throws?
    (intersect-bindings
     :exp
     a
     (get (protocols/bindings (bind (range 0 10) a)) a)
     (get (protocols/bindings (bind (range 20 30) a)) a)))
   := true
  
   ))

(defn merge-with-key
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (f key val-in-result val-in-latter)."
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
			(let [k (key e) v (val e)]
			  (if (contains? m k)
			    (assoc m k (f k (get m k) v))
			    (assoc m k v))))
          merge2 (fn [m1 m2]
		   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))

(extend-protocol protocols/IExpress
  clojure.lang.IPersistentSet
  (write [self] (set (map protocols/write self)))
  (codomain [self] {types/Set self})
  (decisions [self] (->> self
                     (map protocols/decisions)
                     (apply merge-with-key intersect-domains)))
  (bindings [self] (->> self
                     (map protocols/bindings)
                     (apply merge-with-key (partial intersect-bindings self))))
  (validate [self] (doall (map protocols/validate self)) self)
  (translate  [self] (>> {:elements 
                         (apply str (interpose "," (map protocols/translate self)))} 
                        "{{{elements}}}")))

(spec/def ::domain (spec/map-of types/all-decision-types #(boolean (protocols/write %))))
(spec/def ::domainv (spec/coll-of ::domain))
(spec/def ::decisions (spec/nilable (spec/map-of decision? ::domain)))
(spec/def ::binding (spec/tuple (every-pred set? sorted?) #(boolean (protocols/write %))))
(spec/def ::bindings (spec/nilable (spec/map-of decision? ::binding)))

(extend-protocol protocols/IExpress
  Number
  (write [self] self)
  (codomain [self] {types/Numeric self})
  (decisions [_self] nil)
  (bindings [_self] nil)
  (validate [self] self)
  (translate [self] (str self)))

(extend-protocol protocols/IExpress
  Boolean
  (write [self] self)
  (codomain [self] {types/Bool self})
  (decisions [_self] nil)
  (bindings [_self] nil)
  (validate [self] self)
  (translate [self] (str self)))

(extend-protocol protocols/IExpress
  ;; todo is it really helpful to extend this onto object? what would happen if we did not?
  Object
  (write [self] self)
  (codomain [self] (throw (ex-info "unsupported type" {:self self})))
  (decisions [_self] nil)
  (bindings [_self] nil)
  (validate [self] self)
  (translate [self] (throw (ex-info "unsupported type" {:self self})))
  (bindings [self] nil))

#_(extend-protocol protocols/IExpress
 nil
  (write [self] self)
  (codomain [self] {Null self})
  (decisions [_self] nil)
  (validate [self] self))

(defn unify-argv-decisions [expression]
  {:post [(spec/valid? ::decisions %)]}
  (->> (:argv expression)
       ;; this line is only relevant if the arg is a decision. otherwise we're constantly restricting the decision to the domains of its call sites.
       (map (fn [domain arg]
              (if (decidable? arg)
                (merge-with-key
                 intersect-domains
                 (protocols/decisions arg)
                 {(decidable->decision arg) domain})
                (protocols/decisions arg)))
            (protocols/domainv expression))
       (apply merge-with-key intersect-domains)))

(defn validate-domains [expression]
  (doall
   (->> (:argv expression)
        (map protocols/validate)
        (map
         (fn [domain arg]
           (intersect-domains arg (protocols/codomain arg) domain))
         (protocols/domainv expression))))
  (protocols/decisions expression)
  expression)

(defn translate-binary-operation [op-string left right]
  (>> {:left left :right right :op-string op-string}
      "({{left}} {{op-string}} {{right}})"))

(defn translate-nary-operation [op-string args]
  (reduce (partial translate-binary-operation op-string) args))

(comment
  (translate-nary-operation "and" [1 2])
  )

(defn unify-argv-bindings [e]
  (->> (:argv e)
       (map protocols/bindings)
       (apply merge-with-key (partial intersect-bindings e))))

(defrecord TermPlus [argv]
  protocols/IExpress
  (write [_self] (apply list '+ (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (repeat {types/Numeric self}))
  (decisions [self] (unify-argv-decisions self))
  (bindings [self] (unify-argv-bindings self))
  (validate [self] (validate-domains self))
  (translate [self] (translate-nary-operation "+" (map protocols/translate (:argv self)))))

(defrecord TermAnd [argv]
  protocols/IExpress
  (write [_self] (apply list 'and (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Bool self}))
  (decisions [self] (unify-argv-decisions self))
  (bindings [self] (unify-argv-bindings self))
  (validate [self] (validate-domains self))
  (translate [self] (translate-nary-operation "/\\" (map protocols/translate (:argv self)))))

(defn conjunction 
  ([& args]
   (if (> (count args) 1)
     (protocols/validate (->TermAnd args))
     (first args))))

(defn translate-comparator [self op constructor]
  (case (count (:argv self))
    1 (protocols/translate true)
    2 (apply translate-binary-operation op (map protocols/translate (:argv self)))
    (->> (:argv self)
         (partition 2 1)
         (map constructor)
         (apply conjunction)
         protocols/translate)))

(defrecord TermGreaterThanOrEqualTo [argv]
  protocols/IExpress
  (write [_self] (apply list '>= (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Numeric self}))
  (decisions [self] (unify-argv-decisions self))
  (bindings [self] (unify-argv-bindings self))
  (validate [self] (validate-domains self))
  (translate [self] (translate-comparator self ">=" ->TermGreaterThanOrEqualTo)))

(comment
  
  (protocols/translate (expression (>= 1 (fresh) 3)))
  (partition 2 1 [1 2 3 4])
  )

(defrecord TermModulo [argv]
  protocols/IExpress
  (write [_self] (apply list 'mod (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Numeric self} {types/Numeric self}])
  (decisions [self] (unify-argv-decisions self))
  (bindings [self] (unify-argv-bindings self))
  (validate [self] (validate-domains self))
  (translate [self] (apply translate-binary-operation "mod" (map protocols/translate argv))))

(defrecord TermEquals [argv]
  protocols/IExpress
  (write [_self] (apply list '= (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat (zipmap types/all-decision-types (repeat self))))
  (decisions [self] (unify-argv-decisions self))
  (bindings [self] (unify-argv-bindings self))
  (validate [self]
    (when (empty? (->> (:argv self)
                       (map (comp set keys protocols/codomain))
                       (apply clojure.set/intersection)))
      (throw (ex-info "equality testing requires consistent types" {})))
    self)
  (translate [self] (translate-comparator self "=" ->TermEquals)))

(defrecord TermIntersection [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/intersection (map protocols/write argv)))
  (codomain [self] {types/Set self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (unify-argv-decisions self))
  (bindings [self] (unify-argv-bindings self))
  (validate [self] (validate-domains self))
  (translate [self] (translate-nary-operation "intersect" (map protocols/translate (:argv self)))))

(defrecord TermContains [argv]
  protocols/IExpress
  (write [_self] (apply list 'contains? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Set self} {types/Numeric self}])
  (decisions [self] (unify-argv-decisions self))
  (bindings [self] (unify-argv-bindings self))
  (validate [self] (validate-domains self))
  (translate [self] (translate-binary-operation
                     "in"
                     (protocols/translate (second (:argv self)))
                     (protocols/translate (first (:argv self))))))

(comment
  (protocols/translate (expression (contains? (fresh) (fresh))))
  )

(defmulti rewrite* identity)

(defmethod rewrite* + [_] ->TermPlus)

(defmethod rewrite* >= [_] ->TermGreaterThanOrEqualTo)

(defmethod rewrite* = [_] ->TermEquals)

(defmethod rewrite* mod [_] ->TermModulo)

(defmethod rewrite* clojure.set/intersection [_] ->TermIntersection)

(defmethod rewrite* contains? [_] ->TermContains)

(defmethod rewrite* :default [x] x)

(defn rewrite [form-fn]
  (let [ast-constructor-fn (rewrite* form-fn)]
    (if (= form-fn ast-constructor-fn)
      form-fn
      (fn [& args]
        (if (some protocols/decisions args)
          (protocols/validate (ast-constructor-fn args))
          (apply form-fn args))))))

(defmacro and* [& args]
  `(let [args# (list ~@args)]
     (if (some protocols/decisions args#)
       (protocols/validate (->TermAnd args#))
       (and ~@args))))

(comment 
  
  (expression (conjunction 
               (= 6 (fresh))
               (= (+ 3 (fresh)) (+ 1 2))))
  )

(defmacro expression [form]
  ;; todo:: can you rewrite this in a way that doesnt use var-get?
  (clojure.walk/postwalk
   (fn [f]
     (let [macros {'and 'and*
                   'fn 'fn}
           f# (get macros f f)]
       (if  (and (symbol? f)
                 (not (contains? macros f))
                 (resolve f)
                 (not= (rewrite* (var-get (resolve f))) (var-get (resolve f))))
         `(rewrite ~f#)
         f#)))
   form))

(defmacro ?> 
  "The dither operator.
   dith·er - noun: to be indecisive."
  [form]
  `(expression ~form))

(defn fetch [mzn]
  (let [temp-file (doto (java.io.File/createTempFile "petrushka" ".mzn") .deleteOnExit)
        _ (spit temp-file mzn)
        {:keys [exit out err]} (shell/sh "minizinc" (.getAbsolutePath temp-file) "-a")]
    (if (not= exit 0)
      (throw (ex-info err {}))
      (when (not= out "=====UNSATISFIABLE=====\n") ;; todo - grep for this anywhere in the out string. it might be at the end, or be followed by other text
        out))))

(defn ->output [decisions]
  (let [var-string (->> (for [decision (-> decisions keys sort)]
                          (>> {:x (protocols/translate decision)}
                              "\\\"\\({{x}})\\\""))
                        (interpose " ")
                        (apply str))]
    (>> {:x var-string}
        "output([\"[{{x}}]\"]);")))

(defn domain->type [domain]
  {:pre [(spec/valid? ::domain domain)]}
  (first (sort (keys domain))))

(defn decisions->var-declarations [decisions bindings]
  (->> decisions
       (map (fn [[decision domain]]
              (let [set (binding-set (get bindings decision))
                    type (domain->type domain)
                    _ (when (and (= type types/Set) (nil? set))
                        (throw (ex-info (str "unbound set decision: " (protocols/write decision)) {})))
                    env {:range (some-> set protocols/translate)
                         :decision (protocols/translate decision)}
                    >>* (partial >> env)]
                (cond
                  (= type types/Set) (>>* "var set of {{range}}: {{decision}};")
                  (= type types/Numeric) (>>* "var int: {{decision}};")
                  (= type types/Bool) (>>* "var types/Bool: {{decision}};")))))
       sort))

(defmulti detranspile*
  (fn [decisions [decision _out-str]]
    (domain->type (get decisions decision))))

(defmethod detranspile* types/Numeric [_ [_ out-str]]
  (Integer/parseInt out-str))

(defmethod detranspile* types/Bool [_ [_ out-str]]
  (Boolean/parseBoolean out-str))

(defmethod detranspile* types/Set [_ [_ out-str]]
  (if (re-matches #"[0-9]*\.\.[0-9]*" out-str)
    (let [[lower upper] (->> (string/split out-str #"\.\.")
                             (map #(Integer/parseInt %)))]
      (apply sorted-set (range lower (+ 1 upper))))
    (read-string (str "#" out-str))))

(defn detranspile [& [decisions out-str :as args]]
  (def margs args)
  (->> (string/split out-str #"\n")
       first
       read-string
       (interleave (-> decisions keys sort))
       (partition 2)
       (map (partial detranspile* decisions))
       (zipmap (-> decisions keys sort))))

(defn solve [{:keys [all? async?] :as opts}
             constraint
             objective]
  {:pre [(some? constraint)
         (contains? (protocols/codomain constraint) types/Bool)
         (or (nil? objective) (contains? (protocols/codomain objective) types/Numeric))]}
  (let [constraint-str (>> {:e (protocols/translate constraint)}
                           "constraint {{e}};")
        directive-str (if objective
                        (>> {:e (protocols/translate objective)}
                            "solve maximize {{e}};")
                        "solve satisfy;")
        merged-decisions (merge-with-key
                          intersect-domains
                          (protocols/decisions constraint)
                          (when objective (protocols/decisions objective)))
        var-declarations-str (decisions->var-declarations
                              merged-decisions
                              (merge-with-key
                               (partial intersect-bindings nil)
                               (protocols/bindings constraint)
                               (when objective (protocols/bindings objective))))
        output-str (->output merged-decisions)
        mzn (apply str (interpose "\n" (cond-> var-declarations-str
                                         constraint-str (conj constraint-str)
                                         :always (conj output-str directive-str))))
        _ (def mzn mzn)]
    (if *debug*
      (do (spit "scratch/mzn" mzn) mzn)
      ((if async?
         adapter/call-async
         adapter/call-sync)
       all?
       mzn
       (partial detranspile merged-decisions)))))

(comment
  (hyperfiddle.rcf/enable!)
  
  (satisfy 
   {:all? true} 
   (>= 2 (+ (fresh) 3)))
  )

(defmacro satisfy
  ([term]
   `(satisfy ~term {}))
  ([term opts]
   `(solve
     ~opts
     (expression ~term)
     nil)))

(tests
 (tests "constraint must be boolean"
        (utils.test/throws? (satisfy (+ (fresh) 1))) := true
        (utils.test/throws? (satisfy (= (fresh) 1))) := false
        )
 )

(comment
  (let [a (fresh)]
    (solve nil (?> (= 1 (mod (fresh) 12))) nil))
  
  (let [a (fresh)]
    (satisfy (= 1 (mod (fresh) 12))))
  
  (let [a (fresh)] 
    (maximize a))
  


  )

(defmacro maximize
  ([objective constraint]
   `(maximize ~objective ~constraint {}))
  ([objective constraint opts]
   `(solve
     ~opts
     (expression ~constraint)
     (expression ~objective))))

(tests
 (-> (let [a (fresh)]
       (maximize a (and (>= a 3000) (= 3 (mod a 12)))))
     first 
     vals
     boolean) 
 := true


 (tests "objective must be types/Numeric"
        (utils.test/throws? (maximize (= (fresh) 1) true)) := true)

 (tests "constraint is required"
        (utils.test/throws? (maximize (fresh) nil))
        := true)
 (tests "types are unified across the objective and constraint"
        (let [a (fresh)]

          (utils.test/throws? (maximize (+ a 12) (contains? a 12)))
          := true

          (utils.test/throws? (maximize (+ a 12) (contains? #{} a)))
          := false)))

(defn dithered? [x]
  (boolean (protocols/decisions x)))

(tests
 (dithered? (?> (+ (fresh) 1))) := true
 (dithered? (?> (+ 1 1))) := false
 )

(comment
  (hyperfiddle.rcf/enable!)
  (dithered? (?> (+ 1 (fresh))))
  
  (protocols/decisions #{(fresh)})
  supers
  clojure.set/superset?
  
  (satisfy
   {:all? true
    :async? true}
   (contains? (into #{} (range 2 10)) (fresh)))
  ; *, +, !, -, _, '?, <, > and =


  (let [a (fresh)]
    (protocols/bindings (?> (+ 3 
                     (bind (range 12) a)
                     (bind (range 15) a)))))
  
  (maximum)
  (to-array [i f])

  (def ?> 123)
  (def ?= "abc")

  (defn cluster-free []
    (expression (+)))

  (def a (bind (range 10) (fresh)))
  (def b (fresh))
  (satisfy (= #{1 2 3} (clojure.set/intersection a #{1 2 3})))

  (def ?= expression)

  (let [a (fresh)
        b (?> (+ a 4))]
    (get (satisfy (= b 6))
         a))
  
  number?

  (forall [x (fresh)]
          (not= (contains? a)))

  (exists [item set]
          (not= fffff fff))


  (forall [x (fresh)])

  (expression (let [a (fresh)
                    b (expression (+ a 2))]
                (expression (* b 3))))

  (expression
   (let [a (fresh)
         b (expression (+ a 2))]
     (expression (+ b 3))))

  (clojure.set/intersection)

  (sum [item set]
       (+ 1 2 3))


  (for [a b
        :where [(a > 3)]]
    (not=
     (contains? b (+ a 1))
     (contains? b (+ a 2))))

  (map cluster-free? (domain (fresh) (range 0 12)))

  (solve (expression (= 10 (apply + [1 2 3 4 5 (fresh)]))))
  (solve (expression
          (= (clojure.set/intersection (fresh) #{1 2 3 4 5 6 7 8})
             #{1 2 3 4 5 6})))

  2
  )


#_(extend-protocol protocols/IExpress
 clojure.lang.Sequential
  (write [self] (apply list 'list (map protocols/write self)))
  (codomain [self] {Sequential self})
  (decisions [self] (->> (map protocols/decisions self)
                     (apply merge-with-key intersect-domains)))
  (validate [self] (map protocols/validate self)))