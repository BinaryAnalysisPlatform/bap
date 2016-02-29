#!/bin/bash

TARGETS="arm x86 x86_64"
TEST_FILES=".*\(ls\|cp\|rm\|tr\|wc\)$"
LOADERS="llvm bap-elf"

if [ "x$1" != "x" ]; then
    TEST_FILES=$1
fi

echo "Updating signatures"
bap-byteweight update
echo "done"

for target in $TARGETS; do
    git clone --depth=1 https://github.com/BinaryAnalysisPlatform/$target-binaries.git
    files=`find "$target-binaries" -regex $TEST_FILES -type f`
    for loader in $LOADERS; do
        for file in $files; do
            printf '%s:%-70s ' $loader $file;
            bap $file --loader=$loader -d > /dev/null;
            if [ $? -eq 0 ]; then
                echo 'ok';
            else
                echo 'fail';
                exit 1
            fi
        done
    done
done
