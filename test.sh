#!/bin/bash

ERROR=0

test_case() {
    printf '%-70s ' $1;
    readbin $1 > /dev/null;
    if [ $? -eq 0 ]; then
        echo 'ok';
    else
        echo 'fail';
        ERROR=1
    fi;
}

export -f test_case

TARGETS="arm x86"

cd examples
./run.sh
cd ..

for target in $TARGETS; do
    git clone --depth=1 https://github.com/BinaryAnalysisPlatform/$target-binaries.git
    echo "will test the following files:"
    find "$target-binaries" -type f -regex '.*utils_.*'
    echo "starting the test suite"
    find "$target-binaries" -type f -regex '.*utils_.*' | parallel test_case
    rm -rf "$target-binaries"
done

exit $ERROR
