#!/bin/sh

cd examples
./run.sh
cd ..
readbin /bin/ls > /dev/null

git clone https://github.com/BinaryAnalysisPlatform/arm-binaries.git
cd arm-binaries/coreutils

for file in `ls *`; do
    printf "%-30s" $file;
    readbin $file>/dev/null
    if [ $? ]; then
        echo " ok"
    else
        echo " fail"
        exit 1
    fi
done

cd -
rm -rf arm-binaries
