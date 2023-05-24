(ns petrushka.utils.symbol)

(defn normalize-symbol [s]
  (symbol (resolve s)))