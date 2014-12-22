#!/bin/bash


test_case() {
    printf '%-60s ' $1;
    readbin $1 > /dev/null;
    if [ $? -eq 0 ]; then
        echo 'ok';
    else
        echo 'fail';
        exit 1;
    fi;
}

export -f test_file

TARGETS="arm x86"

cd examples
./run.sh
cd ..

for target in $TARGETS; do
    git clone --depth=1 https://github.com/BinaryAnalysisPlatform/$target-binaries.git
    cd $target-binaries
    find -type f -regex '.*utils_.*' | parallel test_case
    cd -
    rm -rf $target-binaries
done
