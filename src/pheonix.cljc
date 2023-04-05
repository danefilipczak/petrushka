(ns pheonix
  (:require [hyperfiddle.rcf :refer [tests]]))

(def Numeric ::numeric)
(def Set ::set)
(def Bool ::boolean)

(def all-var-types 
  #{Numeric 
    Set
    Bool})

(defprotocol IExpress
  (write [self]) 
  (domainv [self])
  (codomain [self]) 
  (cvars [self]) 
  (translate [self])
  (validate [self]))

(defn intersection [s1 s2]
  (if-let [intersection (not-empty (clojure.set/intersection s1 s2))]
    intersection
    (throw (ex-info "types could not be unified" {}))))

(extend-protocol IExpress
  clojure.lang.IPersistentSet
  (write [self] (set (map write self)))
  (codomain [_self] #{Set})
  (cvars [self] (->> self
                     (map cvars)
                     (apply merge-with intersection)))
  (validate [self] (doall (map validate self)) self))

(defrecord CVar [id]
  IExpress
  (write [self] (list 'fresh (:id self)))
  (codomain [_self] all-var-types)
  (cvars [self] {self all-var-types})
  (validate [self] self))

(defn fresh
  ([]
   (fresh (str (gensym))))
  ([id]
   (->CVar id)))

(extend-protocol IExpress
  Number
  (write [self] self)
  (codomain [_self] #{Numeric})
  (cvars [_self] nil)
  (validate [self] self))

(extend-protocol IExpress
  Boolean
  (write [self] self)
  (codomain [_self] #{Bool})
  (cvars [_self] nil)
  (validate [self] self))

(extend-protocol IExpress
 Object
  (write [self] self)
  (codomain [self] (throw (ex-info "unsupported type" {:self self})))
  (cvars [_self] nil)
  (validate [self] self))

(defn unify-argv-vars [expression]
  (->> (:argv expression)
       ;; this line is only relevant if the arg is a cvar. otherwise we're constantly restricting the cvar to the domains of its call sites.
       (map (fn [domain arg]
              (if (instance? CVar arg)
                (merge-with
                 intersection
                 (cvars arg)
                 (zipmap (keys (cvars arg)) (repeat domain)))
                (cvars arg)))
            (domainv expression))
       (apply merge-with intersection)))

(defn validate-domains [expression]
  (doall
   (->> (:argv expression)
        (map (comp codomain validate))
        (map intersection (domainv expression))))
  (cvars expression)
  expression)

(defrecord TermPlus [argv]
  IExpress
  (write [_self] (apply list '+ (map write argv)))
  (codomain [_self] #{Numeric})
  (domainv [_self] (repeat #{Numeric}))
  (cvars [self] (unify-argv-vars self))
  (validate [self] (validate-domains self)))

(defrecord TermGreaterThanOrEqualTo [argv]
  IExpress
  (write [_self] (apply list '>= (map write argv)))
  (codomain [_self] #{Bool})
  (domainv [_self] (repeat #{Numeric}))
  (cvars [self] (unify-argv-vars self))
  (validate [self] (validate-domains self)))

(defrecord TermEquals [argv]
  IExpress
  (write [_self] (apply list '= (map write argv)))
  (codomain [_self] #{Bool})
  (domainv [_self] (repeat all-var-types))
  (cvars [self] (unify-argv-vars self))
  (validate [self] (validate-domains self)))

(defrecord TermIntersection [argv]
  IExpress
  (write [_self] (apply list 'clojure.set/intersection (map write argv)))
  (codomain [_self] #{Set})
  (domainv [_self] (repeat #{Set}))
  (cvars [self] (unify-argv-vars self))
  (validate [self] (validate-domains self)))

(defrecord TermContains [argv]
  IExpress
  (write [_self] (apply list 'contains? (map write argv)))
  (codomain [_self] #{Bool})
  (domainv [_self] [#{Set} #{Numeric}])
  (cvars [self] (unify-argv-vars self))
  (validate [self] (validate-domains self)))

(defrecord TermAnd [argv]
  IExpress
  (write [_self] (apply list 'and (map write argv)))
  (codomain [_self] #{Bool})
  (domainv [_self] (repeat #{Bool}))
  (cvars [self] (unify-argv-vars self))
  (validate [self] (validate-domains self)))

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

(defmacro expression [form]
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

(defmacro satisfy
  ([term]
   `(satisfy {} ~term))
  ([_opts & terms]
   `(expression (and* ~@terms))))

(declare satisfy-all-blocking)
(declare satisfy-all-async)
(declare satisfy-async)
(declare satisfy-blocking)