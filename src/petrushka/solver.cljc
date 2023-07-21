(ns petrushka.solver
  (:require [petrushka.api :as api]
            [petrushka.flattener :as flattener]
            [petrushka.protocols :as protocols]
            [petrushka.types :as types]
            [clojure.string :as string]
            [clojure.spec.alpha :as spec]
            [petrushka.adapter :as adapter]
            [petrushka.utils.string :refer [>>]]))

(def ^:dynamic *debug* false)

(defn domain->type [domain]
  {:pre [(spec/valid? ::api/domain domain)]}
  (first (sort (keys domain))))

(defn ->output [decisions]
  (let [var-string (->> (for [decision (sort-by :id (-> decisions keys))]
                          (>> {:x (protocols/translate decision)}
                              "\\\"\\({{x}})\\\""))
                        (interpose " ")
                        (apply str))]
    (>> {:x var-string}
        "output([\"[{{x}}]\"]);")))

(defn decisions->var-declarations [decisions bindings]
  (->> decisions
       (map (fn [[decision domain]]
              (let [set (api/binding-set (get bindings decision))
                    type (domain->type domain)
                    _ (when (and (= type types/Set) (nil? set))
                        (throw (ex-info (str "unbound set decision: " (protocols/write decision)) {})))
                    env {:range (some-> set protocols/translate)
                         :decision (protocols/translate decision)}
                    >>* (partial >> env)]
                (cond
                  (= type types/Set) (>>* "var set of {{range}}: {{decision}};")
                  (= type types/Numeric) (>>* "var int: {{decision}};")
                  (= type types/Bool) (>>* "var bool: {{decision}};")))))
       sort))

(defmulti detranspile*
  (fn [decisions [decision _out-str]]
    (domain->type (get decisions decision))))

(defmethod detranspile* types/Numeric [_ [_ out-str]]
  (Integer/parseInt out-str))

(defmethod detranspile* types/Bool [_ [_ out-str]]
  (Boolean/parseBoolean out-str))

(defmethod detranspile* types/Set [_ [_ out-str]]
  (if (re-matches #"[0-9]*\.\.[0-9]*" out-str)
    (let [[lower upper] (->> (string/split out-str #"\.\.")
                             (map #(Integer/parseInt %)))]
      (apply sorted-set (range lower (+ 1 upper))))
    (read-string (str "#" out-str))))

(defn detranspile [& [decisions out-str :as args]]
  (def margs args)
  (->> (string/split out-str #"\n")
       first
       read-string
       (interleave (sort-by :id (-> decisions keys)))
       (partition 2)
       (map (partial detranspile* decisions))
       (zipmap (sort-by :id (-> decisions keys)))))

(defn solve [{:keys [all? async?] :as opts}
             constraint
             objective]
  {:pre [(some? constraint)
         (contains? (protocols/codomain constraint) types/Bool)
         (or (nil? objective) (contains? (protocols/codomain objective) types/Numeric))]}
  (let [constraints (flattener/conjuctive-flattening constraint)
        constraint-str (->> constraints
                            (map (fn [constraint] (>> {:e (protocols/translate constraint)}
                                                      "constraint {{e}};")))
                            (interpose "\n")
                            (apply str))
        directive-str (if objective
                        (>> {:e (protocols/translate objective)}
                            "solve maximize {{e}};")
                        "solve satisfy;")
        merged-decisions (apply 
                          api/merge-with-key
                          api/intersect-domains
                          (concat (map api/cacheing-decisions constraints)
                                  (when objective [(api/cacheing-decisions objective)])))
        var-declarations-str (decisions->var-declarations
                              merged-decisions
                              (apply 
                               api/merge-with-key
                               (partial api/intersect-bindings "ignore") ;; as far as i can tell, this arg was partialled in errantly
                               (concat (map protocols/bindings constraints)
                                       (when objective [(protocols/bindings objective)]))))
        output-str (->output merged-decisions)
        mzn (apply str (interpose "\n" (cond-> var-declarations-str
                                         constraint-str (conj constraint-str)
                                         :always (conj output-str directive-str))))
        _ (def mzn mzn)]
    (if *debug*
      (do (spit "scratch/mzn" mzn) mzn)
      ((if async?
         adapter/call-async
         adapter/call-sync)
       all?
       mzn
       (partial detranspile merged-decisions)))))