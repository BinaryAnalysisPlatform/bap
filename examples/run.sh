#!/bin/sh

for file in `ls *.ml`; do
    baptop $file || exit 1
done
