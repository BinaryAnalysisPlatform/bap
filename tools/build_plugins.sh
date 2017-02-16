#!/bin/sh

set -ue

build_plugin() {
    plugin=$1
    if ocamlfind query bap-plugin-$plugin 2>/dev/null
    then
        TMPDIR=`mktemp --directory`
        cd $TMPDIR
        touch $plugin.ml
        bapbuild -package bap-plugin-$plugin $plugin.plugin
        bapbundle update -desc "`ocamlfind query -format "%D" bap-plugin-$plugin`" $plugin.plugin
        bapbundle install $plugin.plugin
        cd -
        rm -rf $TMPDIR
    fi

}


cd plugins

for plugin in `ls`; do
    build_plugin $plugin &
done

cd ..
wait
