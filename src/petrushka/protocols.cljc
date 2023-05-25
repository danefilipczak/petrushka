(ns petrushka.protocols)

(defprotocol IExpress
  (write [self]) 
  (domainv [self])
  (codomain [self]) 
  (decisions [self]) 
  (translate [self])
  (validate [self])
  (bindings [self]))

(defmulti rewrite* identity)

(defmethod rewrite* :default [x] x)

(defmulti rewrite-macro
  (fn [sym]
    (assert (qualified-symbol? sym))
    sym))

(defmethod rewrite-macro :default [x] x)