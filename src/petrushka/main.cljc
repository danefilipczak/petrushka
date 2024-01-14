(ns petrushka.main
  (:require [hyperfiddle.rcf :refer [tests]]
            [petrushka.protocols :as protocols] 
            [petrushka.solver :as solver]
            [petrushka.terms.core]
            [petrushka.terms.introduced :as terms.introduced]
            [petrushka.terms.set]
            [petrushka.types :as types]
            [petrushka.utils.string :refer [>>]]
            [petrushka.utils.test :as utils.test]
            [petrushka.api :as api] 
            :reload))

(comment
  (hyperfiddle.rcf/enable!)
  )

(defn fresh
  "Mint a fresh decision."
  ([]
   (fresh (str (gensym))))
  ([id]
   {:pre [(string? id)]}
   (if (re-matches #"[A-Za-z][A-Za-z0-9_]*" id)
     (api/->Decision id)
     (throw (ex-info
             (>> {:id id}
                 "Invalid identifier: {{id}}. Identifiers should start with a letter and consist only of letters, numbers, and underscores.")
             {})))))

(defmacro satisfy
  ([term]
   `(satisfy ~term {}))
  ([term opts]
   `(solver/solve
     ~opts
     (api/dither ~term)
     nil)))

(tests "satisfy"
 (tests "constraint must be boolean"
        (utils.test/throws? (satisfy (+ (fresh) 1))) := true
        (utils.test/throws? (satisfy (= (fresh) 1))) := false
        ))

(defmacro
  solve-for
  [sym constraint]
  `(let [~sym (fresh)]
     (get
      (satisfy ~constraint)
      ~sym)))

(defmacro maximize
  ([objective constraint]
   `(maximize ~objective ~constraint {}))
  ([objective constraint opts]
   `(solver/solve
     ~opts
     (api/dither ~constraint)
     (api/dither ~objective))))

(tests "maximize"
 (-> (let [a (fresh)]
       (maximize a (clojure.core/and (>= a 3000) (= 11 (mod a 12)))))
     first 
     vals
     boolean) 
 := true

  (mod 2147483639 12)

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

(defmacro ^:introduced ?> 
  "The dither operator.
   dith·er - verb: to be indecisive."
  [form]
  `(api/dither ~form))

(defn conjunction [& args]
  (loop [expr (first args)
         more (rest args)]
    (if (seq more)
      (recur
       (?> 
        (and expr (first more)))
       (rest more))
      expr))
  #_(apply api/conjunction args) ;; todo - though their implementations are the same, calling the impl function makes some tests fail. why?
  )

(defn disjunction [& args]
  (loop [expr (first args)
         more (rest args)]
    (if (seq more)
      (recur
       (?>
        (or expr (first more)))
       (rest more))
      expr)))

(defmacro ^:introduced forall [[bind set-expr] constraint-expr]
  `(let [~bind (api/lexical (fresh))]
     (?> (terms.introduced/forall 
          '~bind  
          [~bind ~set-expr ~constraint-expr]))))

(defmacro ^:introduced for-set [[bind set-expr] generator-expr]
  `(let [~bind (api/lexical (fresh))]
     (?> (terms.introduced/for-set
          '~bind
          [~bind ~set-expr ~generator-expr]))))

(defn dithered? [x]
  (boolean (api/cacheing-decisions x)))

(def bind api/bind)

(defn fresh-set [super]
  (bind super (fresh)))

(tests
 (protocols/decisions 1)
 (dithered? (?> (+ (fresh) 1))) := true
 (dithered? (?> (+ 1 1))) := false
 )