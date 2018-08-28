#!/bin/sh

set -ue

. $(dirname $0)/bap_config

MAX_JOBS=16

build_plugin() {
    plugin=bap_plugin_$1
    TMPDIR=`mktemp -d`
    cd $TMPDIR
    touch $plugin.ml
    bapbuild -package bap-plugin-$1 $plugin.plugin
    DESC=`ocamlfind query -format "%D" bap-plugin-$1`
    CONS=`ocamlfind query -format "%(constraints)" bap-plugin-$1`
    TAGS=`ocamlfind query -format "%(tags)" bap-plugin-$1`
    if [ ! -z "$CONS" ]; then
        bapbundle update -cons "$CONS" $plugin.plugin
    fi
    if [ ! -z "$TAGS" ]; then
        bapbundle update -tags "$TAGS" $plugin.plugin
    fi
    bapbundle update -desc "$DESC" $plugin.plugin
    bapbundle update -name $1 $plugin.plugin

    mv $plugin.plugin $1.plugin
    bapbundle install $1.plugin
    cd -
    rm -rf $TMPDIR
}

waitforjobs() {
    while test $(jobs -p | wc -w) -ge "$1"; do wait -n; done
}

cd plugins

for plugin in `ls`; do
    if ocamlfind query bap-plugin-$plugin 2>/dev/null
    then
        plugin_lib=`ocamlfind query bap-plugin-$plugin`
        if [ -f $PLUGINS_DIR/$plugin.plugin ]
        then
            existing_plugin=$PLUGINS_DIR/$plugin.plugin
            if [ $plugin_lib  -nt $existing_plugin ]
            then
                build_plugin $plugin &
            else
                echo "Not rebuilding $plugin"
            fi
        else
            echo "Building $plugin as it wasn't built yet"
            build_plugin $plugin &
        fi
        waitforjobs $MAX_JOBS
    else
        echo "Not building $plugin as it wasn't selected"
    fi
done

cd ..
wait
