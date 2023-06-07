(ns petrushka.utils.test)

(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

(defmacro try-catchall
  "A cross-platform variant of try-catch that catches all exceptions.
   Does not support finally, and does not need an exception class."
  [& body]
  (let [try-body (butlast body)
        [catch sym & catch-body :as catch-form] (last body)]
    (assert (= catch 'catch))
    (assert (symbol? sym))
    (if (cljs-env? &env)
      `(try ~@try-body (~'catch js/Object ~sym ~@catch-body))
      `(try ~@try-body (~'catch Throwable ~sym ~@catch-body)))))

(defmacro throws? [body]
  `(try-catchall
    ~body
    false
    (catch e# true)))

(def only-val (comp first vals))