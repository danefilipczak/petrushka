(ns petrushka.flattener
  (:require [petrushka.terms.core :as terms.core]
            [petrushka.terms.utils :as terms.utils]
            [petrushka.protocols :as protocols]
            [petrushka.api :as api]))

(defn conjuctive-flattening* [collected node]
  (if (terms.core/conjunctive? node)
    (mapcat 
     (partial conjuctive-flattening* collected) 
     (terms.utils/decendents node))
    [node]))

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
  (if (contains? substitutions node)
    substitutions
    (assoc
     substitutions
     node
     (cond->> (api/->Decision (str "introduced" (gensym)))
       false #_(not-empty (protocols/bindings node))
       (api/bind 
        (apply 
         clojure.set/union 
         (map 
          api/binding-set 
          (vals (protocols/bindings node)))))))))

(defn post-order-traversal [root substitutions node] 
  (if (simple-term? node)
    (update-sub-map 
     substitutions 
     node)
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
          [(get subs node-with-subs) 
           subs]
          subs)))))  

(defn conjuctive-flattening [node]
  (def node node)
  #_(let [[root subs] (post-order-traversal true {} node)
        _ (def subs subs)]
    (conj
     (for [[k v] subs]
       (terms.core/->TermEquals [k v]))
     (terms.core/->TermEquals [root true])))
  [node]
  )

(comment
  (vals (conjuctive-flattening node))
  (protocols/write node)

  (vals (second (post-order-traversal true {} node))) 
  
  
  (:argv true)
  clojure.walk/postwalk

  

  ;; # flattening 
  ;; perform a post-order transversal of the tree
  ;; keep track of a map of sub-expressions -> their introduced fresh vars
  ;; by using the sub-expressions as the keys of the maps, I believe we get common subexpression elimination 
  
  ;; feed that back - possibly by transforming it into a big conjuction and running it through the conjuctive flattener 
  

  (map protocols/write (conjuctive-flattening c))
  
  

  (terms.core/conjunctive? c)
  )