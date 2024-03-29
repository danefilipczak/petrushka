(ns petrushka.types)

(def Numeric ::numeric)
(def Set ::set)
(def Bool ::boolean)
#_(def Sequential ::sequential)
#_(def Null ::null)

(def all-decision-types 
  #{Numeric 
    Set
    Bool
    #_Sequential
    #_Null})

(defn domain->type [domain]
  #_{:pre [(spec/valid? :api/domain domain)]} ;; todo move specs into common ns
  (first (sort (keys domain))))