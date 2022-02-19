(ns petrushka.main
  (:refer-clojure :exclude [find])
  (:require [hyperfiddle.rcf :refer [tests]]
            [failjure.core :as f]
            [petrushka.utils.cvar :refer [cvar?]]
            [clojure.java.shell :as shell]
            [petrushka.analyzer :as analyzer]
            [petrushka.transpiler :as transpiler]))

(defn fetch [mzn]
  (let [temp-file (doto (java.io.File/createTempFile "petrushka" ".mzn") .deleteOnExit)
        _ (spit temp-file mzn)
        {:keys [exit out err]} (shell/sh "minizinc" (.getAbsolutePath temp-file))]
    (if (not= exit 0)
      (f/fail err)
      (if (= out "=====UNSATISFIABLE=====\n")
        (f/fail :unsatisfiable)
        out))))

(defn solve [query]
  (f/attempt-all [cvar-table (analyzer/->cvar-table query)
                  mzn (transpiler/transpile query cvar-table)
                  _ (def mzn mzn)
                  response (fetch mzn)
                  result (transpiler/detranspile response cvar-table)]
                 result
                 (f/when-failed [f]
                                (:message f))))

(defn solve-where [where]
  (solve {:where where}))

(comment 
  
  (solve {:where [[:and [:> :a 100003] [:> :a 3]]]})

  (solve-where [[:and [:> :a 3] [:> :a 5]]])

  (hyperfiddle.rcf/enable!)
  
  )