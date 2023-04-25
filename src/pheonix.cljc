(ns pheonix
  (:require [clojure.java.shell :as shell]
            [clojure.spec.alpha :as spec]
            [clojure.string :as string]
            [hyperfiddle.rcf :refer [tests]]
            [petrushka.utils.string :refer [>>]]
            [clojure.spec.alpha :as s]))

(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

(defmacro try-catchall
  "A cross-platform variant of try-catch that catches all exceptions.
   Does not support finally, and does not need an exception class."
  [& body]
  (let [try-body (butlast body)
        [catch sym & catch-body :as catch-form] (last body)]
    (assert (= catch 'catch))
    (assert (symbol? sym))
    (if (cljs-env? &env)
      `(try ~@try-body (~'catch js/Object ~sym ~@catch-body))
      `(try ~@try-body (~'catch Throwable ~sym ~@catch-body)))))

(defmacro throws? [body]
  `(try-catchall
    ~body
    false
    (catch e# true)))

(def Numeric ::numeric)
(def Set ::set)
(def Bool ::boolean)
#_(def Sequential ::sequential)
#_(def Null ::null)

(def all-var-types 
  #{Numeric 
    Set
    Bool
    #_Sequential
    #_Null})

(defprotocol IExpress
  (write [self]) 
  (domainv [self])
  (codomain [self]) 
  (cvars [self]) 
  (translate [self])
  (validate [self])
  (bindings [self]))

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
                   (str " in expression " (write e)))))]
    (throw (ex-info
            (str "inconsistent model: expression "
                 (write expression)
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
 (intersect-domains 2 {Bool 2} {Bool 2}) := {Bool 2}
 (intersect-domains 2 {Bool 2} {Bool 2 Numeric 2}) := {Bool 2}
 (intersect-domains 2 {Bool 2} {Bool 3}) := {Bool 3} ;; right to left precidence for conflicting keys
 (throws? (intersect-domains 2 {Bool 2} {Numeric 2})) := true
 )

(defn binding-set [x]
  {:pre [(s/valid? (spec/nilable ::binding) x)]}
  (first x))

(defn binding-source [x]
  {:pre [(s/valid? (spec/nilable ::binding) x)]}
  (second x))

(defn binding-error! [cvar binding1 binding2]
  (throw (ex-info
            (str "inconsistent model:  "
                 (write cvar)
                 " is bound to non-intersecting sets "
                 (write (binding-set binding1))
                 " via "
                 (write (binding-source binding1))
                 " and "
                 (write (binding-set binding2))
                 " via "
                 (write (binding-source binding2)))
            {})))

(defrecord CVar [id]
  IExpress
  (write [self] (list 'fresh (:id self)))
  (codomain [self] (zipmap all-var-types (repeat self)))
  (cvars [self] {self (zipmap all-var-types (repeat self))})
  (bindings [self] nil)
  (validate [self] self)
  (translate [self] (str (:id self))))

(defn fresh
  ([]
   (fresh (str (gensym))))
  ([id]
   {:pre [(string? id)]}
   (if (re-matches #"[A-Za-z][A-Za-z0-9_]*" id)
     (->CVar id)
     (throw (ex-info
             (>> {:id id}
                 "Invalid identifier: {{id}}. Identifiers should start with a letter and consist only of letters, numbers, and underscores.")
             {})))))

(defrecord Binding [set cvar]
  IExpress
  (write [self] (list 'bind set (write cvar)))
  (codomain [self] (codomain cvar))
  (cvars [self] (cvars cvar))
  (validate [_self] (validate cvar))
  (translate [_self] (translate cvar))
  (bindings [self] {cvar [set self]}))

(def cvar? (partial instance? CVar))
(def binding? (partial instance? Binding))
(def decidable? (some-fn cvar? binding?))

(defn decidable->cvar [x]
  {:pre [(decidable? x)]
   :post [(cvar? %)]}
  (if (binding? x)
    (:cvar x)
    x))

(defn bind [set cvar]
  {:pre [(cvar? cvar)]}
  (->Binding (apply sorted-set set) cvar))

(defn intersect-bindings
  "given a cvar and two bindings,
   return the intersection of those bindings 
   or throw if their intersection is empty."
  [expression cvar binding1 binding2]
  {:pre [(and (spec/valid? ::binding binding1)
              (spec/valid? ::binding binding2))]
   :post [(spec/valid? ::binding %)]}
  (if-let [intersection (not-empty 
                         (clojure.set/intersection 
                          (first binding1) 
                          (first binding2)))]
    [intersection expression]
    (binding-error! cvar binding1 binding2)))

(tests
 (let [a (fresh)]
   (intersect-bindings
    :exp
    a
    (get (bindings (bind (range 0 10) a)) a)
    (get (bindings (bind (range 0 5) a)) a))
   := [#{0 1 2 3 4} :exp]

   (intersect-bindings
    :exp
    a
    (get (bindings (bind (range 0 10) a)) a)
    (get (bindings (bind (range 0 10) a)) a))
   := [#{0 1 2 3 4 5 6 7 8 9} :exp]

   (throws?
    (intersect-bindings
     :exp
     a
     (get (bindings (bind (range 0 10) a)) a)
     (get (bindings (bind (range 20 30) a)) a)))
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

(extend-protocol IExpress
  clojure.lang.IPersistentSet
  (write [self] (set (map write self)))
  (codomain [self] {Set self})
  (cvars [self] (->> self
                     (map cvars)
                     (apply merge-with-key intersect-domains)))
  (bindings [self] (->> self
                     (map bindings)
                     (apply merge-with-key (partial intersect-bindings self))))
  (validate [self] (doall (map validate self)) self)
  (translate  [self] (>> {:elements 
                         (apply str (interpose "," (map translate self)))} 
                        "{{{elements}}}")))

(spec/def ::domain (spec/map-of all-var-types #(boolean (write %))))
(spec/def ::domainv (spec/coll-of ::domain))
(spec/def ::cvars (spec/nilable (spec/map-of cvar? ::domain)))
(spec/def ::binding (spec/tuple (every-pred set? sorted?) #(boolean (write %))))
(spec/def ::bindings (spec/nilable (spec/map-of cvar? ::binding)))

(extend-protocol IExpress
  Number
  (write [self] self)
  (codomain [self] {Numeric self})
  (cvars [_self] nil)
  (bindings [_self] nil)
  (validate [self] self)
  (translate [self] (str self)))

(extend-protocol IExpress
  Boolean
  (write [self] self)
  (codomain [self] {Bool self})
  (cvars [_self] nil)
  (bindings [_self] nil)
  (validate [self] self)
  (translate [self] (str self)))

(extend-protocol IExpress
  ;; todo is it really helpful to extend this onto object? what would happen if we did not?
  Object
  (write [self] self)
  (codomain [self] (throw (ex-info "unsupported type" {:self self})))
  (cvars [_self] nil)
  (bindings [_self] nil)
  (validate [self] self)
  (translate [self] (throw (ex-info "unsupported type" {:self self})))
  (bindings [self] nil))

#_(extend-protocol IExpress
 nil
  (write [self] self)
  (codomain [self] {Null self})
  (cvars [_self] nil)
  (validate [self] self))

(defn unify-argv-vars [expression]
  {:post [(spec/valid? ::cvars %)]}
  (->> (:argv expression)
       ;; this line is only relevant if the arg is a cvar. otherwise we're constantly restricting the cvar to the domains of its call sites.
       (map (fn [domain arg]
              (if (decidable? arg)
                (merge-with-key
                 intersect-domains
                 (cvars arg)
                 {(decidable->cvar arg) domain})
                (cvars arg)))
            (domainv expression))
       (apply merge-with-key intersect-domains)))

(defn validate-domains [expression]
  (doall
   (->> (:argv expression)
        (map validate)
        (map (fn [domain arg]
               (intersect-domains arg (codomain arg) domain)) 
             (domainv expression))))
  (cvars expression)
  expression)

(defn translate-binary-operation [op-string left right]
  (>> {:left left :right right :op-string op-string}
      "({{left}}{{op-string}}{{right}})"))

(defn translate-nary-operation [op-string args]
  (reduce (partial translate-binary-operation op-string) args))

(comment
  (translate-nary-operation "and" [1 2])
  )

(defn unify-argv-bindings [e]
  (->> (:argv e)
       (map bindings)
       (apply merge-with-key (partial intersect-bindings e))))

(defrecord TermPlus [argv]
  IExpress
  (write [_self] (apply list '+ (map write argv)))
  (codomain [self] {Numeric self})
  (domainv [self] (repeat {Numeric self}))
  (cvars [self] (unify-argv-vars self))
  (bindings [self] (unify-argv-bindings self))
  (validate [self] (validate-domains self))
  (translate [self] (translate-nary-operation "+" (map translate (:argv self)))))

(defrecord TermAnd [argv]
  IExpress
  (write [_self] (apply list 'and (map write argv)))
  (codomain [self] {Bool self})
  (domainv [self] (repeat {Bool self}))
  (cvars [self] (unify-argv-vars self))
  (bindings [self] (unify-argv-bindings self))
  (validate [self] (validate-domains self))
  (translate [self] (translate-nary-operation "/\\" (map translate (:argv self)))))

(defn conjunction 
  ([& args]
   (if (> (count args) 1)
     (validate (->TermAnd args))
     (first args))))

(defn translate-comparator [self op constructor]
  (case (count (:argv self))
    1 (translate true)
    2 (apply translate-binary-operation op (map translate (:argv self)))
    (->> (:argv self)
         (partition 2 1)
         (map constructor)
         (apply conjunction)
         translate)))

(defrecord TermGreaterThanOrEqualTo [argv]
  IExpress
  (write [_self] (apply list '>= (map write argv)))
  (codomain [self] {Bool self})
  (domainv [self] (repeat {Numeric self}))
  (cvars [self] (unify-argv-vars self))
  (bindings [self] (unify-argv-bindings self))
  (validate [self] (validate-domains self))
  (translate [self] (translate-comparator self ">=" ->TermGreaterThanOrEqualTo)))

(comment
  
  (translate (expression (>= 1 (fresh) 3)))
  (partition 2 1 [1 2 3 4])
  )

(defrecord TermEquals [argv]
  IExpress
  (write [_self] (apply list '= (map write argv)))
  (codomain [self] {Bool self})
  (domainv [self] (repeat (zipmap all-var-types (repeat self))))
  (cvars [self] (unify-argv-vars self))
  (bindings [self] (unify-argv-bindings self))
  (validate [self]
    (when (empty? (->> (:argv self)
                       (map (comp set keys codomain))
                       (apply clojure.set/intersection)))
      (throw (ex-info "equality testing requires consistent types" {})))
    self)
  (translate [self] (translate-comparator self "=" ->TermEquals)))

(defrecord TermIntersection [argv]
  IExpress
  (write [_self] (apply list 'clojure.set/intersection (map write argv)))
  (codomain [self] {Set self})
  (domainv [self] (repeat {Set self}))
  (cvars [self] (unify-argv-vars self))
  (bindings [self] (unify-argv-bindings self))
  (validate [self] (validate-domains self))
  (translate [self] (translate-nary-operation " intersect " (map translate (:argv self)))))

(defrecord TermContains [argv]
  IExpress
  (write [_self] (apply list 'contains? (map write argv)))
  (codomain [self] {Bool self})
  (domainv [self] [{Set self} {Numeric self}])
  (cvars [self] (unify-argv-vars self))
  (bindings [self] (unify-argv-bindings self))
  (validate [self] (validate-domains self))
  (translate [self] (translate-binary-operation
                     " in "
                     (translate (second (:argv self)))
                     (translate (first (:argv self))))))

(comment
  (translate (expression (contains? (fresh) (fresh))))
  )

(defmulti rewrite* identity)

(defmethod rewrite* + [_] ->TermPlus)

(defmethod rewrite* >= [_] ->TermGreaterThanOrEqualTo)

(defmethod rewrite* = [_] ->TermEquals)

(defmethod rewrite* clojure.set/intersection [_] ->TermIntersection)

(defmethod rewrite* contains? [_] ->TermContains)

(defmethod rewrite* :default [x] x)

(defn rewrite [form-fn]
  (let [ast-constructor-fn (rewrite* form-fn)]
    (if (= form-fn ast-constructor-fn)
      form-fn
      (fn [& args]
        (if (some cvars args)
          (validate (ast-constructor-fn args))
          (apply form-fn args))))))

(defmacro and* [& args]
  `(let [args# (list ~@args)]
     (if (some cvars args#)
       (validate (->TermAnd args#))
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

(defmacro ?> [form]
  `(expression ~form))

(defn fetch [mzn]
  (let [temp-file (doto (java.io.File/createTempFile "petrushka" ".mzn") .deleteOnExit)
        _ (spit temp-file mzn)
        {:keys [exit out err]} (shell/sh "minizinc" (.getAbsolutePath temp-file))]
    (if (not= exit 0)
      (throw (ex-info err {}))
      (when (not= out "=====UNSATISFIABLE=====\n") ;; todo - grep for this anywhere in the out string. it might be at the end, or be followed by other text
        out))))

(defn ->output [cvars]
  (let [var-string (->> (for [cvar (-> cvars keys sort)]
                          (>> {:x (translate cvar)}
                              "\\\"\\({{x}})\\\""))
                        (interpose " ")
                        (apply str))]
    (>> {:x var-string}
        "output([\"[{{x}}]\"]);")))

(defn domain->type [domain]
  {:pre [(spec/valid? ::domain domain)]}
  (first (sort (keys domain))))

(defn cvars->var-declarations [cvars bindings]
  (->> cvars
       (map (fn [[cvar domain]]
              (let [set (binding-set (get bindings cvar))
                    type (domain->type domain)
                    _ (when (and (= type Set) (nil? set))
                        (throw (ex-info (str "unbound set cvar: " (write cvar)) {})))
                    env {:range (translate set)
                         :cvar (translate cvar)}
                    >>* (partial >> env)]
                (cond
                  (= type Set) (>>* "var set of {{range}}: {{cvar}};")
                  (= type Numeric) (>>* "var int: {{cvar}};")
                  (= type Bool) (>>* "var bool: {{cvar}};")))))
       sort))

(defmulti detranspile*
  (fn [cvars [cvar _out-str]]
    (domain->type (get cvars cvar))))

(defmethod detranspile* Numeric [_ [_ out-str]]
  (Integer/parseInt out-str))

(defmethod detranspile* Bool [_ [_ out-str]]
  (Boolean/parseBoolean out-str))

(defmethod detranspile* Set [_ [_ out-str]]
  (if (re-matches #"[0-9]*\.\.[0-9]*" out-str)
    (let [[lower upper] (->> (string/split out-str #"\.\.")
                             (map #(Integer/parseInt %)))]
      (apply sorted-set (range lower (+ 1 upper))))
    (read-string (str "#" out-str))))

(defn detranspile [& [out-str cvars :as args]]
  (def margs args)
  (->> (string/split out-str #"\n")
       first
       read-string
       (interleave (-> cvars keys sort))
       (partition 2)
       (map (partial detranspile* cvars))
       (zipmap (-> cvars keys sort))))

(defn solve [expression]
  (let [constraint (>> {:e (translate expression)}
                       "constraint {{e}};")
        var-declarations (cvars->var-declarations 
                          (cvars expression)
                          (bindings expression))
        output (->output (cvars expression))
        mzn (apply str (interpose "\n" (conj var-declarations constraint output)))
        _ (def mzn mzn)
        response (fetch mzn)
        result (detranspile response (cvars expression))]
    result))

(defmacro satisfy
  ([term]
   `(satisfy {} ~term))
  ([_opts & terms]
   `(solve (expression (and* ~@terms)))))

(comment
  
  (satisfy (contains? (fresh) 26))
  ; *, +, !, -, _, '?, <, > and =


  (let [a (fresh)]
    (bindings (?> (+ 3 
                     (bind (range 12) a)
                     (bind (range 15) a)))))
  
  (maximum)
  (to-array [i f])

  (def ?> 123)
  (def ?= "abc")

  (defn cluster-free []
    (expression (+)))

  (def a (fresh))
  (def b (fresh))
  (satisfy (= #{1 2 3} (clojure.set/intersection a #{1 2 3})))

  (def ?= expression)

  (let [a (fresh)
        b (?> (+ a 4))]
    (get (satisfy (= b 6))
         a))

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

  (def bind)

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

(declare satisfy)
(declare satisfy-all)
(declare maximize)
(declare maximize-all)


#_(extend-protocol IExpress
 clojure.lang.Sequential
  (write [self] (apply list 'list (map write self)))
  (codomain [self] {Sequential self})
  (cvars [self] (->> (map cvars self)
                     (apply merge-with-key intersect-domains)))
  (validate [self] (map validate self)))