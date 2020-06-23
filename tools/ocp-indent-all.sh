#!/bin/sh

# a regex for files to indent
files_to_indent='.*\.\(ml\|mli\|mll\|mly\)$'

VERSION=$(ocp-indent --version)

if [ -z $VERSION ]; then
    echo "Please install ocp-indent"
    exit 1
else
    echo "Reindenting with ocp-indent $VERSION"
fi

if $(git diff --quiet --exit-code); then
    git ls-files | grep -e $files_to_indent | while read file
    do
        ocp-indent -i $file
    done
else
    echo "Please commit your code before running this script"
    exit 1
fi
