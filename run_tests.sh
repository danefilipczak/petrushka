#!/bin/sh

echo "Running JVM tests"
clojure -X:test :dirs "[\"test\" \"src\"]" 

#:patterns "[\"example.*\" \"petrushka.*-test\"]"