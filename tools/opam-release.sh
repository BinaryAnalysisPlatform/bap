#!/bin/sh

# OPAM Release Script
#
# Usage `opam-release release`
# Goes through all packages in the current working dir and releases
# the latest version of every package that has version 'master', then
# deletes the master version of the released package
#
# The master package must have a url to its upstream repository in the
# url file. The url shall have the `path/name#master` form,
# otherwise it will be skipped. The repository shall have tags of the
# form `v<version>`.  The tag name with the latest version (the latest
# defined by the order induced by the `sort` utility) will be used to
# get the released archive file and the new version number.
#
# After the release we will lint all packages in the repository


# check all packages
lint_packages() {
    for file in packages/*/*/opam; do
        opam lint $file
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
    sed -i "s/{= \"master\"}/{= \"$new\"}/" $newpath/opam
    git add $newpath
}

# add_mirror github-name package version
add_mirror() {
    url=packages/$2/$2.$3/url
    if [ $1 = "bap" ]; then
        mirror_url="https://mirrors.aegis.cylab.cmu.edu/bap/$3/v$3.tar.gz"
        cat >> $url <<EOF
mirrors: [
         "$mirror_url"
]
EOF
    git add $url
    fi
}

# geturl pkgdir
# prints an unquoted URL of the package repo, or prints nothing
# if the package doesn't have the url file, or url doesn't point
# to a m
geturl() {
    url=$1/url
    if [ -f $1/url ]; then
        perl -n -e '/src: "(.*)#master"/ && print $1' $url
    fi
}

# gitclone repos url
gitclone() {
    repo=$1
    url=$2
    if [ ! -d $repo ]; then
        cd `dirname $repo`
        git clone --quiet $url
        cd $OLDPWD
    fi
}

# latest_version repo
latest_version() {
    cd $1
    git tag | sort -r | head -n1 | perl -n -e '/v?(.*)/ && print $1'
    cd $OLDPWD
}

# archive repo version
# prints a path to the archive file
archive() {
    echo "$1/archive/v$2.tar.gz" | sed 's/git/https/'
}

# md5sum dir repo version
getmd5sum() {
    url=`archive $2 $3`
    pkg=`basename $2`
    file=`printf "%s/%s-%s" $1 $pkg $3`
    if [ ! -f $file ]; then
        curl --silent -L $url > $file
    fi
    md5sum $file | cut -d' ' -f1
}

# release_main_packages $old $new $url $md5
release_master_packages() {
    tmp=`mktemp -d`
    for pkg_path in packages/*/*.master; do
        url=`geturl $pkg_path`
        if [ "no$url"  != "no" ]; then
            pkg=`basename $pkg_path .master`
            git_name=`basename $url`
            repo=`printf "%s/%s" $tmp $git_name`
            gitclone $repo $url
            next_version=`latest_version $repo`
            checksum=`getmd5sum $tmp $url $next_version`
            tarbal=`archive $url $next_version`
            release_package $pkg master $next_version $tarbal $checksum
            add_mirror $git_name $pkg $next_version
        else
            echo "skipping $pkg: no master url"
        fi
    done
    rm -rf $tmp
}

delete_master_packages() {
    for pkg_path in packages/*/*.master; do
        git rm -r $pkg_path
    done
}

delete_nonmaster_packages() {
    for pkg_path in packages/*/*; do
        version=`echo $pkg_path | perl -ne '/.*?\.(.*)/ && print "$1"'`
        conf=`echo $pkg_path | perl -ne '/conf-.*/ && print "conf" '`
        if [ "is_$version" != "is_master" -a "is_$conf" != "is_conf" ]; then
            git rm -rf $pkg_path
        fi
    done
}

release() {
    release_master_packages
    delete_master_packages
    lint_packages
}


"$@"
