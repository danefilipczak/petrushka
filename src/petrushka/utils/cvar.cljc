(ns petrushka.utils.cvar
  (:require [petrushka.operations :as ops]))

(defn cvar? [e]
  (and (keyword? e)
       (not (contains? (set (keys ops/all)) e))))

(comment
  (cvar? :+) := false
  (cvar? :not-an-op) := true
  )