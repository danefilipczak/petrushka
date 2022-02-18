    "my freedom will be so much the greater and more meaningful the more narrowly I limit my field of action and the more I surround myself with obstacles. Whatever diminishes constraint, diminishes strength. The more constraints one imposes, the more one frees one’s self of the chains that shackle the spirit." - Igor Stravinsky

## Petrushka is a library for solving constraint problems in clojure / script.
It aims to provide:
- expressive, idiomatic syntax
- out of the box solving with minizinc compiled to wasm, on both the JVM and browser
https://github.com/renatoathaydes/wasm-on-jvm/tree/master/examples/configured-c-to-wasm
https://flexiana.com/2021/11/wasm-part-i-what-is-webassembly-reasons-to-take-it-seriously
https://hasgeek.com/inclojure/2020/sub/wasm-on-clojure-6CuxGMcGY4otVcGgyJSBF7
https://github.com/helins/wasm.cljc
- type inference
- helpful error messages





```clojure
(p/solve-where [[:contains (range 0 6) :a]
                [:= 3 [:+ 2 :a]])
=> {:a 1}
```

### inspirations, similarities
[clojure2minizinc](https://github.com/tanders/clojure2minizinc)
- A clojure interface to minizinc that provides more features, but a less idiomatic syntax. For those already familiar with programming in minizinc, clojure2minizinc may be more flexible, but requires an external dependency on the minizinc compiler / solver utility.

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