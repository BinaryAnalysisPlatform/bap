#!/bin/sh

set -ue

. $(dirname $0)/bap_config

md5sum=md5sum

if [ $OS = "macosx" ]; then
    md5sum=md5
fi

built_plugins=_build/built_plugins
[ -d $built_plugins ] || mkdir -p $built_plugins

compute_digest() {
    plugin_lib=$1
    output=$2
    $md5sum $(ocamlfind query -predicates byte -a-format -r $plugin_lib) > $output
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
    if [ -d $plugin ]; then
        (build_plugin $plugin)&
    fi
done
wait
echo "Finished updating plugins"
