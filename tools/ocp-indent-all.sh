#!/bin/sh

# a Emacs regex for files to indent
files_to_indent='.*\.\(ml\|mli\|mll\|mly\)'

VERSION=$(ocp-indent --version)

if [ -z $VERSION ]; then
    echo "Please install ocp-indent"
    exit 1
else
    echo "Reindenting with ocp-indent $VERSION"
fi

if $(git diff --quiet --exit-code); then
    find -type f -regex $files_to_indent -exec ocp-indent -i '{}' \;
else
    echo "Please commit your code before running this script"
    exit 1
fi
