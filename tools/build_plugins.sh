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
        DESC=`ocamlfind query -format "%D" bap-plugin-$plugin`
        TAGS=`ocamlfind query -format "%(tags)" bap-plugin-$plugin`
        if [ -z "$TAGS" ]; then
            bapbundle update -desc "$DESC" $plugin.plugin
        else
            bapbundle update -desc "$DESC" -tags "$TAGS" $plugin.plugin
        fi
        if [ -f $plugin/resources ]; then
            cd $plugin
            for line in `cat resources`; do
                bapbundle update -add-resources $line ../$plugin.plugin
            done
            cd ..
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
