- remove external dependency on minizinc through out of the box solving with minizinc compiled to wasm, on both the JVM and browser
    - https://github.com/renatoathaydes/wasm-on-jvm/tree/master/examples/configured-c-to-wasm
    - https://flexiana.com/2021/11/wasm-part-i-what-is-webassembly-reasons-to-take-it-seriously
    - https://hasgeek.com/inclojure/2020/sub/wasm-on-clojure-6CuxGMcGY4otVcGgyJSBF7
    - https://github.com/helins/wasm.cljc

- configurable throw on error
- explore whether it may be more effective for the where clause to be a single boolean expression rather than a vector of implied conjunctive expressions. The sytactic equivilent would be to add an :and to the outer vector, and we can explore the performance implications of compiling an outer :and to a series of 'constraint' expressions. Determine the difference between these two approaches in compiled flatzinc.  
