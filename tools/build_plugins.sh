#!/bin/sh

set -ue


build_plugin() {
    plugin=$1
    if ocamlfind query bap-plugin-$plugin 2>/dev/null
    then
        TMPDIR=`mktemp -d`
        cd $TMPDIR
        touch $plugin.ml
        bapbuild -package bap-plugin-$plugin $plugin.plugin
        DESC=`ocamlfind query -format "%D" bap-plugin-$plugin`
        CONS=`ocamlfind query -format "%(constraints)" bap-plugin-$plugin`
        TAGS=`ocamlfind query -format "%(tags)" bap-plugin-$plugin`
        if [ ! -z "$CONS" ]; then
            bapbundle update -cons "$CONS" $plugin.plugin
        fi
        if [ ! -z "$TAGS" ]; then
            bapbundle update -tags "$TAGS" $plugin.plugin
        fi
        bapbundle update -desc "$DESC" $plugin.plugin

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
