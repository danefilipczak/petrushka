(ns petrushka.solver
  (:require [petrushka.api :as api]
            [petrushka.flattener :as flattener]
            [petrushka.protocols :as protocols]
            [petrushka.types :as types]
            [clojure.string :as string]
            [clojure.spec.alpha :as spec]
            [petrushka.utils.log :as log]
            [petrushka.adapter :as adapter]
            [petrushka.utils.string :refer [>>]]))

(def ^:dynamic *debug* false)

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
                    type (types/domain->type domain)
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
    (types/domain->type (get decisions decision))))

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

(defn expand-all [node] 
  ;; todo this is very slow, and potentially incorrect
  ;; what if a term expands to something that itself needs expansion, like an if?
  ;; please correct
  (clojure.walk/prewalk
   (fn [n]
     (if
      (satisfies? protocols/IExpand n)
       (protocols/expand n)
       n))
   node))

(defn solve [{:keys [all? async?] :as opts}
             constraint
             objective]
  {:pre [(some? constraint)
         (contains? (protocols/codomain constraint) types/Bool)
         (or (nil? objective) (contains? (protocols/codomain objective) types/Numeric))]}
  (let [model-decisions (api/merge-with-key
                         api/intersect-domains
                         (api/cacheing-decisions constraint)
                         (when objective (api/cacheing-decisions objective)))
        constraint-with-forced-decisions-and-expanded-terms (clojure.walk/postwalk
                                                             (fn [x]
                                                               (cond 
                                                                 (and (api/decision? x) 
                                                                      (get model-decisions x)) ;; some decisions in the tree are 'local' in the case of introduced statements like forall
                                                                 (api/force-type
                                                                  x
                                                                  (types/domain->type
                                                                   (get model-decisions x)))
                                                                 
                                                                 (satisfies? protocols/IExpand x)
                                                                 (expand-all x)

                                                                 :else x))
                                                             constraint) 
        #_#_constraints (flattener/conjuctive-flattening
                     constraint-with-forced-decisions-and-expanded-terms)
        constraints [constraint-with-forced-decisions-and-expanded-terms]
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
                          model-decisions
                          (map api/cacheing-decisions constraints))
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

#_(defn fetch [mzn]
      (let [temp-file (doto (java.io.File/createTempFile "petrushka" ".mzn") .deleteOnExit)
            _ (spit temp-file mzn)
            {:keys [exit out err]} (shell/sh "minizinc" (.getAbsolutePath temp-file) "-a")]
           (if (not= exit 0)
               (throw (ex-info err {}))
               (when (not= out "=====UNSATISFIABLE=====\n") ;; todo - grep for this anywhere in the out string. it might be at the end, or be followed by other text
                     out))))