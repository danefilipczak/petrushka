    "my freedom will be so much the greater and more meaningful the more narrowly I limit my field of action and the more I surround myself with obstacles. Whatever diminishes constraint, diminishes strength. The more constraints one imposes, the more one frees one’s self of the chains that shackle the spirit." - Igor Stravinsky

## Petrushka is a library for solving constraint problems in clojure.
It aims to provide:
- expressive, idiomatic syntax
- compatability with a range of backend solvers
- type inference
- helpful error messages

```clojure
(p/solve-where [[:in :a (range 0 6)]
                [:= 3 [:+ 2 :a]])
=> {:a 1}
```

### inspirations, similarities
[clojure2minizinc](https://github.com/tanders/clojure2minizinc)
- A clojure interface to minizinc that provides more features, but a less idiomatic syntax. For those already familiar with programming in minizinc, clojure2minizinc may be more flexible.

[comment]: <> (beware: below this line will be overwritten. see utils.docs/generate-readme!)
# Operations
| op | returns | args |
| --- | --- | --- |
|:+|:number|:number:number:&:numbers|
|:in|:boolean|:number:set|
|:not|:boolean|:boolean|
|:and|:boolean|:boolean:&:booleans|
|:set=|:boolean|:set:set:&:sets|
|:=|:boolean|:number:number:&:numbers|
|:if|:any|:boolean:any:any|