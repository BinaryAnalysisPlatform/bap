#!/bin/bash


cd examples
./run.sh
cd ..

echo "Updating signatures"
bap-byteweight update
echo "done"

for target in $TEST_TARGETS; do
    git clone --depth=1 https://github.com/BinaryAnalysisPlatform/$target-binaries.git
    files=`find "$target-binaries" \( -name "$1*" -o -name "gcc_$1*" \) -type f`
    for file in $files; do
        printf '%-70s ' $file;
        bap-objdump $file --loader=$BAP_LOADER -d bil > /dev/null;
        if [ $? -eq 0 ]; then
            echo 'ok';
        else
            echo 'fail';
            exit 1
        fi
    done
done
