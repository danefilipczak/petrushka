(ns petrushka.api
  (:require [clojure.java.shell :as shell]
            [clojure.spec.alpha :as spec]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.set]
            [hyperfiddle.rcf :refer [tests]]
            [petrushka.adapter :as adapter]
            [petrushka.protocols :as protocols]
            [petrushka.types :as types]
            [petrushka.utils.string :refer [>>]]
            [petrushka.utils.symbol :as symbols]
            [petrushka.utils.test :as utils.test]
            ))

(declare cacheing-validate)
(declare cacheing-decisions)

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
  (bindings [self] (when-let [r (::range (meta self))]
                     {self [r self]}))
  (validate [self] self)
  (translate [self] (str (:id self))))

(def decision? (partial instance? Decision))

(defn bind [super decision]
  "Constrains a decision, presumably a set decision, to be a subset of super.
   Referentially transparent - evaluates to the decision.
   No-op when used with decisions of other types."
  {:pre [(decision? decision)]}
  (with-meta decision {::range (apply sorted-set super)}))

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
                     (map cacheing-decisions)
                     (apply merge-with-key intersect-domains)))
  (bindings [self] (->> self
                     (map protocols/bindings)
                     (apply merge-with-key (partial intersect-bindings self))))
  (validate [self] (doall (map cacheing-validate self)) self)
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
  (validate [self] (mapv cacheing-validate self))
  (decisions [self] (->> self
                     (map cacheing-decisions)
                     (apply merge-with-key intersect-domains))))

(defn unify-argv-decisions [expression]
  {:post [(spec/valid? ::decisions %)]}
  (->> (:argv expression)
       ;; this line is only relevant if the arg is a decision. otherwise we're constantly restricting the decision to the domains of its call sites.
       (map (fn [domain arg]
              (if (decision? arg)
                (merge-with-key
                 intersect-domains
                 (cacheing-decisions arg)
                 {arg domain})
                (cacheing-decisions arg)))
            (protocols/domainv expression))
       (apply merge-with-key intersect-domains)))

(defn validate-domains [expression]
  (doall
   (->> (:argv expression)
        (map cacheing-validate)
        (map
         (fn [domain arg]
           (intersect-domains arg (protocols/codomain arg) domain))
         (protocols/domainv expression))))
  (cacheing-decisions expression)
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
        (if (some cacheing-decisions args)
          (cacheing-validate (ast-constructor-fn args))
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

(declare walk)

(defn walk-let* [locals form]
  (let [kvs (partition 2 (second form))
        new-locals (into #{} (map first kvs))]
    (apply
     list
     (first form)
     (into [] (mapcat (fn [[k v]] [k (walk locals v)]) kvs))
     (walk 
      (clojure.set/union locals new-locals)
      (drop 2 form)))))

(defn walk-fn* [locals form]
  (let [[invocation bodies] (split-with (complement sequential?) form)
        bodies (if (vector? (first bodies))
                 (list bodies) bodies)]
    (apply
     list
     (concat
      invocation
      (for [[params content] bodies]
        (list
         params
         (walk
          (clojure.set/union locals (into #{} params))
          content)))))))

(defn walk
  [locals form]
  (let [walk* (partial walk locals)]
    (cond
    ;; functions
      (contains? locals form) form
      (function-sym? form) `(rewrite-fn ~form)

    ;; macros
      (and (list? form)
           (macro-sym? (first form)))
      (if (or (introduced? (first form))
              (contains? locals (first form)))
        (apply list (map walk* form))
        (let [qualified-symbol (symbols/fully-qualify-symbol (first form))
              ast-constructor-fn (protocols/rewrite-macro qualified-symbol)]
          (if (not= qualified-symbol ast-constructor-fn)
            `(if (some cacheing-decisions ~(vec (map walk* (rest form))))
               (cacheing-validate (~ast-constructor-fn ~(vec (map walk* (rest form)))))
               ~(apply list (map walk* form)))
            (walk* (apply list (macroexpand-1 form))))))

    ;; special forms
      (and (list? form)
           (symbol? (first form))
           (not (contains? locals (first form))))
      (cond (contains? #{'loop* 'let*} (first form))
            (walk-let* locals form)

            (= 'fn* (first form))
            (walk-fn* locals form)

            :else
            (let [ast-constructor-fn (protocols/rewrite-symbol (first form))]
              (if (not= (first form) ast-constructor-fn)
                `(if (some cacheing-decisions ~(vec (map walk* (rest form))))
                   (cacheing-validate (~ast-constructor-fn ~@(map walk* (rest form))))
                   ~(apply list (map walk* form)))
                (apply list (map walk* form)))))

    ;; recur 

      (list? form) (apply list (map walk* form))

      (instance? clojure.lang.IMapEntry form)
      (clojure.lang.MapEntry/create (walk* (key form)) (walk* (val form)))

      (seq? form)
      (doall (map walk* form))

      (instance? clojure.lang.IRecord form)
      (reduce (fn [r x] (conj r (walk* x))) form form)

      (coll? form)
      (into (empty form) (map walk* form))

      :else form)))


(defmacro dither [form]
  (walk #{} form))

(defn conjunction [& args]  
  (loop [expr (first args)
         more (rest args)]
    (if (seq more)
      (recur
       (dither 
        (and expr (first more)))
       (rest more))
      expr)))

(defn translate-comparator [self op function]
  (case (count (:argv self))
    1 (protocols/translate true)
    2 (apply 
       translate-binary-operation 
       op 
       (map protocols/translate (:argv self)))
    (->> (:argv self)
         (partition 2 1)
         (map (fn [[a b]]
                ((rewrite-fn function) a b)))
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

(def cacheing-validate (memoize protocols/validate))
(def cacheing-decisions (memoize protocols/decisions))