(ns petrushka.utils.symbol)

(defn fully-qualify-symbol [s]
  (symbol (resolve s)))