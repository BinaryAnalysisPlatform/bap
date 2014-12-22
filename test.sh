#!/bin/sh

TARGETS="arm x86"

cd examples
./run.sh
cd ..

for target in $TARGETS; do
    git clone --depth=1 https://github.com/BinaryAnalysisPlatform/$target-binaries.git

    for file in `find -type f -regex '.*utils_.*'`; do
        printf '%-60s ' $file;
        readbin $file > /dev/null;
        if [ $? -eq 0 ]; then
            echo 'ok';
        else
            echo 'fail';
            exit 1;
        fi;
    done

    cd -
    rm -rf $target-binaries
done
