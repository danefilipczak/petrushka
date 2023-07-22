(ns petrushka.flattener
  (:require [petrushka.terms.core :as terms.core]
            [petrushka.terms.utils :as terms.utils]
            [petrushka.protocols :as protocols]
            [petrushka.api :as api]
            [petrushka.types :as types]))

(defn simple-term? [node]
  (not-any? terms.utils/decendents (terms.utils/decendents node)))

(defn replace-children-recursive [subs node]
  (def xargs [subs node])
  (clojure.walk/postwalk
   (fn [x] 
     (if (coll? x)
       (get subs x x)
       x)) 
   node))

(comment
  (apply replace-children-recursive xargs)
  
  )

(defn update-sub-map [substitutions node]
  (let [preserve? (or
                   (contains? substitutions node)
                   ;; an identical expression has been substituted elsewhere in the tree
                   ;; allowing the existing substitution to remain is a form of common subexpression elimination 

                   (contains?
                    (set (keys (protocols/codomain node)))
                    types/Set)
                   ;; substitution of sets requires forwarding of bindings. skip for now... 
                   )]
    (if preserve?
      substitutions
      (let [type (types/domain->type 
                  (protocols/codomain node))]
        (assoc
         substitutions
         node
         (api/force-type 
          (api/->Decision (str "introduced" (gensym)))
          type))))))

(defn post-order-traversal [root substitutions node] 
  (if (simple-term? node)
    (if root
      [node {}]
      (update-sub-map 
       substitutions 
       node))
    (let [substitutions' (reduce 
                          (partial post-order-traversal false) 
                          substitutions 
                          (terms.utils/decendents 
                           node))]
      (let [node-with-subs (replace-children-recursive
                            substitutions'
                            node)
            subs (update-sub-map
                  substitutions'
                  node-with-subs)]
        (if root 
          [(get subs node-with-subs) subs]
          subs)))))  

(defn conjuctive-flattening [node]
  (def node node)
  (let [[root subs] (post-order-traversal true {} node)
        _ (def subs subs)]
    (conj
     (for [[k v] subs]
       (terms.core/->TermEquals [k v]))
     root)))

(comment
  (protocols/write (conjuctive-flattening node))
  (vals )
  (protocols/write node)

  (vals (second (post-order-traversal true {} node))) 
  
  
  (:argv true)
  clojure.walk/postwalk

  

  ;; Flattening Process:

  ;; # unification - analyses
;; Getting all decision types from the tree.
  ;; # reification - supplimented AST
;; Walking through the tree and replacing decisions with forced-variable versions  
  ;; replacing nodes with expanded versions of themselves
  
;; Performing a post-order traversal, replacing expressions with variables that are forced to belong to the codomain of the expression.
;; Keeping a map of sub-expressions and the newly introduced fresh variables.
;; Achieving common subexpression elimination by using the sub-expressions as keys in the map.
  ;; optimization - unrolling compound booleans into separate statements. (this can be acheived by implementing conjunctive translation in terms of 'expand') 
  

  (map protocols/write (conjuctive-flattening c))
  
  

  (terms.core/conjunctive? c)
  )