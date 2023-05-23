(ns petrushka.adapter
  (:require [clojure.core.async :as async]
            [hyperfiddle.rcf :as rcf :refer [tests tap %]]
            [clojure.string :as string])
  (:import [java.io File StringWriter BufferedReader InputStreamReader]))

(defn env []
  #?(:clj :clj :cljs :cljs))

(defn system-text [x]
  (boolean
   (or (string/includes? x "===")
       (string/includes? x "---")
       (string/includes? x "UNSATISFIABLE"))))

(defn filtering-chan
  ([]
   (filtering-chan identity))
  ([xfn]
   (async/chan 
    1 
    (comp 
     (filter (complement system-text))
     (map (or xfn identity))))))

(defmulti call-minizinc 
  (fn [env _chan _mzn _all?] env))

(defmethod call-minizinc :clj
  [_env chan mzn all?]
  (let [temp-file (doto (java.io.File/createTempFile "petrushka" ".mzn") .deleteOnExit)
        _ (spit temp-file mzn)
        args (cond-> ["minizinc"]
                 all? (conj "-a")
                 :always (conj (.getAbsolutePath temp-file)))
        proc (.exec
              (Runtime/getRuntime)
              (into-array String args))]
    (with-open [stdout (.getInputStream proc)
                out-reader (BufferedReader. (InputStreamReader. stdout))
                stderr (.getErrorStream proc)
                err-writer (StringWriter.)]
      (doall (for [line (line-seq out-reader)]
               (async/go (async/>! chan line))))
      (clojure.java.io/copy stderr err-writer)
      (async/close! chan)
      (let [exit (.waitFor proc)
            error (.toString err-writer)]
        (when (not= exit 0)
          (throw (ex-info error {})))))))

#?(:clj
   (tests
    (let [c (filtering-chan)]
      (async/go-loop [x (async/<! c)]
        (tap x)
        (when x
          (recur (async/<! c))))
      (call-minizinc
       :clj
       c
       "var int: x; constraint x>3; constraint x<6;"
       true)
      (boolean (#{"x = 4;" "x = 5;"} %)) := true
      (boolean (#{"x = 4;" "x = 5;"} %)) := true
      % := nil)))

(defn call-sync
  [all? mzn xfn]
  (let [chan (filtering-chan xfn)
        _ (future (call-minizinc (env) chan mzn all?))
        solutions (async/<!!
                   (async/go-loop [solutions []]
                     (if-let [solution (async/<! chan)]
                       (recur (conj solutions solution))
                       solutions)))]
    (if all? solutions (first solutions))))

(tests
 (let [c (call-sync
          true
          "var int: x; constraint x>3; constraint x<6;"
          nil)]
   (every? #{"x = 4;" "x = 5;"} c) := true)
 
 (let [c (call-sync
          false
          "var int: x; constraint x>3; constraint x<6;"
          nil)]
   (boolean (#{"x = 4;" "x = 5;"} c)) := true))

(defn call-async
  [all? mzn xfn]
  (let [chan (filtering-chan xfn)]
    (future (call-minizinc (env) chan mzn all?))
    chan))

(tests
 (let [c (call-async
          true
          "var int: x; constraint x>3; constraint x<6;"
          nil)]
   (async/go-loop [x (async/<! c)]
     (tap x)
     (when x
       (recur (async/<! c))))
   (boolean (#{"x = 4;" "x = 5;"} %)) := true
   (boolean (#{"x = 4;" "x = 5;"} %)) := true
   % := nil))

