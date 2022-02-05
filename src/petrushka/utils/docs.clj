(ns petrushka.utils.docs
  (:require [petrushka.main :as main]))

(defn pluralize-keyword [k]
  (-> k
      name
      (vector "s")
      ((partial apply str))
      keyword))

(defn generate-readme! []
  (let [lines (-> (slurp "readme.md")
                  (clojure.string/split #"\n"))
        good-lines (take-while (partial not= "# Operations") lines)
        operation-table (let [rows (for [[op [return-type [low-airity high-airity] [left-type right-type]]] main/ops]
                                     [op
                                      return-type
                                      (cond-> (take (+ (- (or high-airity low-airity) low-airity)
                                                       low-airity)
                                                    (conj (repeat (or right-type left-type)) left-type))

                                        (nil? high-airity)
                                        (concat [:& (pluralize-keyword (or right-type left-type))]))])
                              header "| op | returns | args |\n| --- | --- | --- |"]
                          (apply str (interpose "\n" (concat [header]
                                                             (map
                                                              (fn [[op return args]]
                                                                (str "|" op "|" return "|" (apply str args) "|"))
                                                              rows)))))]
    (spit "readme.md" (apply str (interpose "\n" (concat good-lines ["# Operations" operation-table]))))))

(comment
  (generate-readme!)
  )