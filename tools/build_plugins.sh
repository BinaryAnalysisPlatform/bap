#!/bin/sh

# TODO rewrite it as an ocaml script for portability

set -uex

cd plugins

for plugin in `ls`; do
    if ocamlfind query bap-plugin-$plugin 2>/dev/null
    then
        touch $plugin.ml
        bapbuild -package bap-plugin-$plugin $plugin.plugin
        bapbundle update -desc "`ocamlfind query -format "%D" bap-plugin-$plugin`" $plugin.plugin
        if [ -f $plugin/resources ]; then
            cd $plugin
            for line in `cat resources`; do
                bapbundle update -add-resources $line ../$plugin.plugin
            done
            cd ..
        fi
        bapbundle install $plugin.plugin
        bapbuild -clean
        rm $plugin.ml
    fi
done

cd ..
