#!/bin/sh

set -u

. $(dirname $0)/bap_config

MAX_JOBS=8
md5sum=md5sum

if [ $OS = "macosx" ]; then
    md5sum=md5
fi

built_plugins=_build/built_plugins
mkdir -p $built_plugins

compute_digest() {
    plugin_lib=$1
    output=$2
    $md5sum $(ocamlfind query -predicates byte -a-format -r $plugin_lib) > $output
}

feature() {
    (export $1; eval $(cat setup.data); printenv $1)
}

is_set() {
    [ "is-$(feature $1)" = "is-true" ]
}


is_enabled() {
    [ -n "$(feature omake)" ] || is_set everything || is_set $1
}

jobs_folder=_build/jobs
rm -rf $jobs_folder
mkdir -p $jobs_folder

count_jobs() {
    ls -1 $jobs_folder | wc -l
}

start_job() {
    realpath $(mktemp -p $jobs_folder XXX)
}

finish_job() {
    rm $1
}

build_plugin() {
    plugin=bap_plugin_$1
    plugin_lib=bap-plugin-$1
    BUILDIR=$built_plugins/$1

    [ -d $BUILDIR ] || mkdir $BUILDIR
    cd $BUILDIR

    [ -f digest ] || touch digest

    compute_digest $plugin_lib new_digest

    if cmp -s digest new_digest
    then
        echo "$1: is up-to-date"
    else
        sync
        touch $plugin.ml
        bapbuild -clean
        bapbuild -package bap-plugin-$1 $plugin.plugin
        DESC=$(ocamlfind query -format "%D" bap-plugin-$1)
        CONS=$(ocamlfind query -format "%(constraints)" bap-plugin-$1)
        TAGS=$(ocamlfind query -format "%(tags)" bap-plugin-$1)
        if [ -z $CONS ]; then
            bapbundle update -name $1 -desc "$DESC" -tags "core,$TAGS" $plugin.plugin
        else
            bapbundle update -name $1 -desc "$DESC" -cons "$CONS" -tags "core,$TAGS" $plugin.plugin
        fi
        mv $plugin.plugin $1.plugin
    fi
    bapbundle install $1.plugin
    mv new_digest digest
}

for plugin in $(ls plugins); do
    if [ -d plugins/$plugin ] && is_enabled $plugin; then
        ID=$(start_job)
        while [ $(count_jobs) -gt $MAX_JOBS ]; do
            wait || exit 1
        done
        (build_plugin $plugin; finish_job $ID)&
    fi
done
wait || exit 1
echo "Finished updating plugins"
