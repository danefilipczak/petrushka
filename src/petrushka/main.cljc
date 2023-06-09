(ns petrushka.main
  (:require [petrushka.api :as api]
            [hyperfiddle.rcf :refer [tests]]
            [petrushka.utils.test :as utils.test]
            [petrushka.utils.symbol :as symbols]
            [petrushka.protocols :as protocols]
            [petrushka.utils.string :refer [>>]]
            ;; for defmethods
            [petrushka.terms.core]
            [petrushka.terms.set]
            [petrushka.terms.introduced :as terms.introduced]))

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
   `(api/solve
     ~opts
     (api/expression ~term)
     nil)))

(tests "satisfy"
 (tests "constraint must be boolean"
        (utils.test/throws? (satisfy (+ (fresh) 1))) := true
        (utils.test/throws? (satisfy (= (fresh) 1))) := false
        ))

(defmacro maximize
  ([objective constraint]
   `(maximize ~objective ~constraint {}))
  ([objective constraint opts]
   `(api/solve
     ~opts
     (api/expression ~constraint)
     (api/expression ~objective))))

(tests "maximize"
 (-> (let [a (fresh)]
       (maximize a (clojure.core/and (>= a 3000) (= 3 (mod a 12)))))
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

(defmacro ^:introduced ?> 
  "The dither operator.
   dith·er - noun: to be indecisive."
  [form]
  `(api/expression ~form))

(defn conjunction [& args]
  (if (seq (rest args))
    (?> 
     (and 
      (first args) 
      (apply conjunction (rest args))))
    (first args)))

(defmacro ^:introduced forall [[bind set-expr] constraint-expr]
  `(let [~bind (fresh)]
     (?> (terms.introduced/forall 
          '~bind  
          [~bind ~set-expr ~constraint-expr]))))

(defn dithered? [x]
  (boolean (api/cacheing-decisions x)))

(tests
 (protocols/decisions 1)
 (dithered? (?> (+ (fresh) 1))) := true
 (dithered? (?> (+ 1 1))) := false
 )