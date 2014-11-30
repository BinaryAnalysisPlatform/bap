#!/bin/sh
oasis setup
./configure --prefix=$(opam config var prefix)
make 
make reinstall
