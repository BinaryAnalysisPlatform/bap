#!/bin/sh

# A crude tool or, more precisely, a toolkit for releasing BAP
# packages to opam. We hope, that at some of time this simple
# shell script will evolve to a fully automated solution.

GITHUB="https://github.com/BinaryAnalysisPlatform"

# This a list of all packages that are distributed via our main
# repository (bap.git).
main_pkgs="\
bap
bap-abi \
bap-api \
bap-arm \
bap-beagle \
bap-byteweight \
bap-byteweight-frontend \
bap-c \
bap-cache \
bap-callsites \
bap-demangle \
bap-dump-symbols \
bap-dwarf \
bap-elf \
bap-frontc \
bap-frontend \
bap-fsi-benchmark \
bap-future \
bap-ida \
bap-ida-plugin \
bap-llvm
bap-mc \
bap-microx \
bap-objdump \
bap-phoenix \
bap-piqi \
bap-print \
bap-std \
bap-symbol-reader \
bap-taint \
bap-taint-propagator \
bap-term-mapper \
bap-trace \
bap-traces \
bap-warn-unused \
bap-x86 \
graphlib \
regular \
text-tags \
"

conf_pkgs="\
conf-binutils \
conf-ida \
conf-llvm
"


rest="\
bap-ida-python \
bap-veri \
bap-frames \
bap-server \
core-lwt \
"

# lint pkg version
lint() {
    for pkg in $1; do
        opam lint packages/$pkg/$pkg.$2/opam
    done
}


# ``release_package pkg old new url md5`` release a pkg by copying
#  existing description and bumping version and archive url.
release_package() {
    pkg=$1
    old=$2
    new=$3
    url=$4
    md5=$5
    oldpath=packages/$pkg/$pkg.$old
    newpath=packages/$pkg/$pkg.$new

    echo "releasing $pkg from $oldpath to $newpath..."
    mkdir -p $newpath
    cp -r $oldpath/* $newpath
    if [ $# -gt 3 ]; then
    cat > $newpath/url <<EOF
archive: "$url"
checksum: "$md5"
EOF
    fi
    sed -i "s/^version:.*/version: \"$new\"/" $newpath/opam
    git add $newpath
}

# release_main_packages $old $new $url $md5
release_main_packages() {
    for pkg in $main_pkgs; do
        release_package $pkg $1 $2 $3 $4
    done
}


remove_main_packages() {
    for pkg in $main_pkgs; do
        git rm -rf packages/$pkg/$pkg.$1
    done
}


# Example:
# release_main_packages "1.0.0" "1.1.0" \
#                       "$GITHUB/bap/archive/v1.1.0.tar.gz" \
#                       "92e7f703d58ce1835bfeeed9ec523242"
