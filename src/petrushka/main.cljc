(ns petrushka.main
  (:require [petrushka.api :as api]
            [hyperfiddle.rcf :refer [tests]]
            [petrushka.utils.test :as utils.test]
            ;; for defmethods
            [petrushka.terms.core]))

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
        (utils.test/throws? (satisfy (+ (api/fresh) 1))) := true
        (utils.test/throws? (satisfy (= (api/fresh) 1))) := false
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
 (-> (let [a (api/fresh)]
       (maximize a (and (>= a 3000) (= 3 (mod a 12)))))
     first 
     vals
     boolean) 
 := true


 (tests "objective must be types/Numeric"
        (utils.test/throws? (maximize (= (api/fresh) 1) true)) := true)

 (tests "constraint is required"
        (utils.test/throws? (maximize (api/fresh) nil))
        := true)
 (tests "types are unified across the objective and constraint"
        (let [a (api/fresh)]

          (utils.test/throws? (maximize (+ a 12) (contains? a 12)))
          := true

          (utils.test/throws? (maximize (+ a 12) (contains? #{} a)))
          := false)))