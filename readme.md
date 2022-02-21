"my freedom will be so much the greater and more meaningful the more narrowly I limit my field of action and the more I surround myself with obstacles. Whatever diminishes constraint, diminishes strength. The more constraints one imposes, the more one frees one’s self of the chains that shackle the spirit." - Igor Stravinsky

## Petrushka is a DSL for solving constraint problems in clojure.

It works by transpiling clojure data structures to minizinc, and aims to provide:
- expressive syntax that is idiomatic to clojure.
- type inference where possible
- helpful error messages

Solving requires that the minizinc compiler and solver are present in your path under the normal alias - `minizinc`.

```clojure
(solve {:find {:a [:number] ;; declare a type
               :b [:set (range 10)]} ;; and an optional range
        :where [[:contains? :b :a]
                [:> :a 5]
                [:< :a 8]
                [:contains? #{7 8} :a]]})
=> {:a 7, :b #{0 1 2 3 4 5 6 7 8 9}}

;; solve without declaring types or ranges. Safe for integers, treats sets as empty sets when no range can be infered.
(solve-where [[:= :a 1] [:and [:> :b 2] [:< :b 4]]]) 
=> {:a 1 :b 3})
```

### inspirations, similarities
[clojure2minizinc](https://github.com/tanders/clojure2minizinc)
- A clojure interface to minizinc that provides more features and a syntax that closely resembles the corresponding minizinc code. For those already familiar with programming in minizinc, clojure2minizinc may offer more features.

[comment]: <> (beware: below this line will be overwritten. see utils.docs/generate-readme!)
# Operations
| op | returns | args |
| --- | --- | --- |
|:<=|:boolean|:number:&:numbers|
|:when|:boolean|:boolean:boolean|
|:>|:boolean|:number:&:numbers|
|:if|:boolean|:boolean:boolean:boolean|
|:-|:number|:number:number:&:numbers|
|:or|:boolean|:boolean:&:booleans|
|:not|:boolean|:boolean|
|:true?|:boolean|:boolean|
|:>=|:boolean|:number:&:numbers|
|:iff|:boolean|:boolean:boolean:&:booleans|
|:xor|:boolean|:boolean:boolean:&:booleans|
|:+|:number|:number:number:&:numbers|
|:false?|:boolean|:boolean|
|:and|:boolean|:boolean:&:booleans|
|:contains?|:boolean|:set:number|
|:=|:boolean|:number:&:numbers|
|:<|:boolean|:number:&:numbers|
|:set=|:boolean|:set:set:&:sets|
