#!/bin/sh
cargo build
for i in tests/*.lox
do
    fn=${i#tests/}
    fn=${fn%.lox}
    ./target/debug/loxwalk $i > ${i%.lox}.txt

    if cmp -s tests/$fn.txt tests/expected/$fn.txt
    then
        echo $i PASSED
        rm tests/$fn.txt
    else
        echo $i FAILED
        echo
        diff tests/$fn.txt tests/expected/$fn.txt
    fi
done
