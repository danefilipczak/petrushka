(ns petrushka.protocols)

(defprotocol IExpress
  (write [self]) 
  (domainv [self])
  (codomain [self]) 
  (decisions [self]) 
  (translate [self])
  (validate [self])
  (bindings [self]))

(defprotocol IExpand
  (expand [self]))

(defmulti rewrite-function identity)

(defmethod rewrite-function :default [x] x)

(defmulti rewrite-macro
  (fn [sym]
    (assert (qualified-symbol? sym))
    sym))

(defmethod rewrite-macro :default [x] x)

(defmulti rewrite-symbol identity)

(defmethod rewrite-symbol :default [x] x)