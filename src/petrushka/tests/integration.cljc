(ns petrushka.tests.integration
  (:require [hyperfiddle.rcf :refer [tests]]
            [petrushka.main :refer [solve-where solve]]))

(tests
 (tests
  "basic numerical operations."
  (solve-where [[:= :a 1]]) := {:a 1}
  (solve-where [[:= :a 1] [:and [:> :b 2] [:< :b 4]]]) := {:a 1 :b 3})
 
 (tests
  (solve {:find {:a [:number]
                 :b [:set (range 10)]}
          :where [[:contains? :b :a]
                  [:> :a 5]
                  [:< :a 8]
                  [:contains? #{7 8} :a]]})
  := {:a 7, :b #{0 1 2 3 4 5 6 7 8 9}}
  )
 )