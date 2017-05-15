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

        if [ ! -z "$CONS" ] && [ ! -z "$TAGS" ]; then
            bapbundle update -desc "$DESC" -cons "$CONS" -tags "$TAGS" $plugin.plugin
        elif [ ! -z "$CONS" ]; then
            bapbundle update -desc "$DESC" -cons "$CONS" $plugin.plugin
        elif [ ! -z "$TAGS" ]; then
            bapbundle update -desc "$DESC" -tags "$TAGS" $plugin.plugin
        else
            bapbundle update -desc "$DESC" $plugin.plugin
        fi

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
