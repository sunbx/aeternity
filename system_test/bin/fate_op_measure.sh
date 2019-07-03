#!/bin/sh


./_build/local/bin/aefateasm ~/aeternity/aebytecode/test/asm_code/$1_loop.fate -o $1_loop.fate.bc


for i in 1 10 100 1000 10000 100000
do
   TIME=`./_build/local/bin/aefate ./$1_loop.fate.bc -t  "run($i)"  | grep "Execution Time" | awk '{print $3}'`
   printf "%10s, %8d, %16d\n"  "\"$1\"" $i $TIME
done
