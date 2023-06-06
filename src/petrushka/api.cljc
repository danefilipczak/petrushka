(ns petrushka.api
  (:require [clojure.java.shell :as shell]
            [clojure.spec.alpha :as spec]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [hyperfiddle.rcf :refer [tests]]
            [petrushka.adapter :as adapter]
            [petrushka.protocols :as protocols]
            [petrushka.types :as types]
            [petrushka.utils.string :refer [>>]]
            [petrushka.utils.symbol :as symbols]
            [petrushka.utils.test :as utils.test]))

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
 ;; this needs to be moved into a test ns so that it can call main/fresh
 #_(let [a (fresh)]
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

(spec/def ::domain (spec/map-of types/all-decision-types #(some? (protocols/write %))))
(spec/def ::domainv (spec/coll-of ::domain))
(spec/def ::decisions (spec/nilable (spec/map-of decision? ::domain)))
(spec/def ::binding (spec/tuple (every-pred set? sorted?) #(some? (protocols/write %))))
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

(extend-protocol protocols/IExpress
  nil
  (write [self] self)
  (decisions [self] self)
  (bindings [self] self)
  (validate [self] self)
  (codomain [self] self)
  (translate [self] (throw (ex-info "unsupported type" {:self self}))))

(extend-protocol protocols/IExpress
  clojure.lang.IPersistentVector
  (write [self] (mapv protocols/write self))
  (validate [self] (mapv protocols/validate self))
  (decisions [self] (->> self
                     (map protocols/decisions)
                     (apply merge-with-key intersect-domains))))

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

(defn rewrite-fn [form-fn]
  (let [ast-constructor-fn (protocols/rewrite-function form-fn)]
    (if (= form-fn ast-constructor-fn)
      form-fn
      (fn [& args]
        (if (some protocols/decisions args)
          (protocols/validate (ast-constructor-fn args))
          (apply form-fn args))))))

(defn fn-inspect
  "returns true if x is a symbol that resolves to a fn whose var satisfies var-predicate"
  [var-predicate x]
  (when (symbol? x)
    (when-let [v (resolve x)]
      (when-let [value (clojure.test/get-possibly-unbound-var v)]
        (and (fn? value)
             (var-predicate v))))))

(def macro-sym? (partial fn-inspect (comp boolean :macro meta)))
(def function-sym? (partial fn-inspect (comp not :macro meta)))
(def introduced? (partial fn-inspect (comp :introduced meta)))

(defmacro expression [form]
  (clojure.walk/postwalk
   (fn [f]
     (cond
       
       ;; macros
       (and (list? f)
            (macro-sym? (first f)))
       (if (introduced? (first f))
         f
         (let [ast-constructor-fn (protocols/rewrite-macro (symbols/fully-qualify-symbol (first f)))]
           (if (not= (first f) ast-constructor-fn)
             `(if (some protocols/decisions ~(vec (rest f)))
                (protocols/validate (~ast-constructor-fn ~(vec (rest f))))
                ~f)
             f)))

       ;; functions
       (function-sym? f) `(rewrite-fn ~f)

       ;; special forms
       (and (list? f)
            (symbol? (first f)))
       (let [ast-constructor-fn (protocols/rewrite-symbol (first f))]
           (if (not= (first f) ast-constructor-fn)
             `(if (some protocols/decisions ~(vec (rest f)))
                (protocols/validate (~ast-constructor-fn ~@(rest f)))
                ~f)
             f))

       :else f))
   form))

(defn conjunction [& args]
  (if (seq (rest args))
    (expression 
     (and 
      (first args) 
      (apply conjunction (rest args))))
    (first args)))

(defn translate-comparator [self op constructor]
  (case (count (:argv self))
    1 (protocols/translate true)
    2 (apply translate-binary-operation op (map protocols/translate (:argv self)))
    (->> (:argv self)
         (partition 2 1)
         (map constructor)
         (apply conjunction)
         protocols/translate)))

(defn fetch [mzn]
  (let [temp-file (doto (java.io.File/createTempFile "petrushka" ".mzn") .deleteOnExit)
        _ (spit temp-file mzn)
        {:keys [exit out err]} (shell/sh "minizinc" (.getAbsolutePath temp-file) "-a")]
    (if (not= exit 0)
      (throw (ex-info err {}))
      (when (not= out "=====UNSATISFIABLE=====\n") ;; todo - grep for this anywhere in the out string. it might be at the end, or be followed by other text
        out))))

(defn ->output [decisions]
  (let [var-string (->> (for [decision (sort-by :id (-> decisions keys))]
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
       (interleave (sort-by :id (-> decisions keys)))
       (partition 2)
       (map (partial detranspile* decisions))
       (zipmap (sort-by :id (-> decisions keys)))))

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


#_(extend-protocol protocols/IExpress
 clojure.lang.Sequential
  (write [self] (apply list 'list (map protocols/write self)))
  (codomain [self] {Sequential self})
  (decisions [self] (->> (map protocols/decisions self)
                     (apply merge-with-key intersect-domains)))
  (validate [self] (map protocols/validate self)))