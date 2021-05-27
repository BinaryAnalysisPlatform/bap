#!/bin/sh

set -e

sudo apt-get update
sudo apt-get install alien autoconf --yes

BAP_VERSION=$1
echo "version is $BAP_VERSION"


GITHUB=https://github.com/BinaryAnalysisPlatform/
SOURCE=$GITHUB/bap
BINDINGS=$GITHUB/bap-bindings
BINARIES="bap bapbundle bapbuild bap-mc"
PREFIX=/usr/local
ARCH=$(dpkg-architecture -qDEB_BUILD_ARCH)
CONFDIR=$PREFIX/etc/bap
TMPDIR=$(mktemp -d)

eval $(opam config env)
echo OCaml is at $(which ocaml)
echo "Looking in the dev-repo for the current list of dependencies"
opam pin add bap --dev-repo --yes -n
echo "Installing System dependenices"
opam depext bap --yes
echo "Installing OCaml dependenices"
opam install --yes --deps-only bap
echo "Installed dependencies. Cleaning up..."
opam pin remove bap

echo "Cloning a fresh repo"
[ -d bap-repo ] || git clone $SOURCE bap-repo
echo "Installing ocamlfind to the system path"

[ -f $PREFIX/bin/ocamlfind ] && cp $PREFIX/bin/ocamlfind $TMPDIR/stored-ocamlfind
sudo cp $(which ocamlfind) $PREFIX/bin


cd bap-repo
LLVM_VERSION=$(opam config var conf-bap-llvm:package-version)
LLVM_CONFIG=$(opam config var conf-bap-llvm:config)

SIGURL=https://github.com/BinaryAnalysisPlatform/bap/releases/download/v2.3.0
echo BAP version is $BAP_VERSION
echo LLVM is $LLVM_VERSION

sed -i "s/-j 2/-j 4/" oasis/common

./configure --enable-everything \
            --disable-ida \
            --with-llvm-version=$LLVM_VERSION \
            --with-llvm-config=$LLVM_CONFIG \
            --libdir=$PREFIX/lib/bap \
            --plugindir=$PREFIX/lib/bap \
            --prefix=$PREFIX \
            --sysconfdir=$CONFDIR

make
sudo sh -c "PATH=$PATH make reinstall"
cd ..

echo "Packing a bap debian package"

sudo rm -rf bap

mkdir -p bap/bap_$BAP_VERSION/DEBIAN

SHARED=bap/bap_$BAP_VERSION/$PREFIX/share
BINDIR=bap/bap_$BAP_VERSION/$PREFIX/bin
LIBDIR=bap/bap_$BAP_VERSION/$PREFIX/lib/bap
DEBIAN=bap/bap_$BAP_VERSION/DEBIAN

SIGDIR=$SHARED/bap/signatures/

mkdir -p $SHARED $BINDIR $LIBDIR $DEBIAN

cp -r $PREFIX/share/bap $SHARED/bap

for binary in $BINARIES; do
    cp $PREFIX/bin/$binary bap/bap_$BAP_VERSION/$PREFIX/bin
done;

cp $PREFIX/lib/bap/*.plugin $LIBDIR

echo "Installing Byteweight signatures"
mkdir -p $SIGDIR
curl -L $SIGURL/sigs.zip > $SIGDIR/byteweight.zip

cat > $DEBIAN/control <<EOF
Package: bap
Architecture: $ARCH
Maintainer: Ivan Gotovchits
Depends: libgmp10, zlib1g, libstdc++6, libtinfo5
Priority: optional
Version: $BAP_VERSION
Description: Binary Analysis Platform
EOF

sudo chown -R root:root bap/bap_$BAP_VERSION
dpkg-deb --build bap/bap_$BAP_VERSION

echo "now building the bindings"

opam install ctypes ctypes-foreign --yes

[ -d bap-bindings ] || git clone $BINDINGS bap-bindings


cd bap-bindings
git pull
autoconf
./configure
make
cd ..


echo "Now packing libbap.deb"
LIBDIR=bap/libbap_$BAP_VERSION/$PREFIX/lib
DEBIAN=bap/libbap_$BAP_VERSION/DEBIAN

mkdir -p $LIBDIR $DEBIAN


cp bap-bindings/_build/bap/libbap.so $LIBDIR/libbap.so.$BAP_VERSION


cat > $DEBIAN/control <<EOF
Package: libbap
Architecture: $ARCH
Maintainer: Ivan Gotovchits
Depends: libgmp10, zlib1g, libstdc++6, libffi6, libtinfo5
Priority: optional
Version: $BAP_VERSION
Description: Binary Analysis Platform C Library
EOF

cat > $DEBIAN/postinst <<EOF
#!/bin/sh
sudo ldconfig
EOF

cat > $DEBIAN/postrm <<EOF
#!/bin/sh
sudo ldconfig
EOF

chmod a+x $DEBIAN/postinst
chmod a+x $DEBIAN/postrm
sudo chown -R root:root bap/libbap_$BAP_VERSION
dpkg-deb --build bap/libbap_$BAP_VERSION

echo "Now packing libbap-dev"
HDRDIR=bap/libbap-dev_$BAP_VERSION/$PREFIX/include
DEBIAN=bap/libbap-dev_$BAP_VERSION/DEBIAN

mkdir -p $DEBIAN $HDRDIR
cp bap-bindings/_build/bap/generated/bap.h $HDRDIR

cat > $DEBIAN/control <<EOF
Package: libbap-dev
Architecture: $ARCH
Maintainer: Ivan Gotovchits
Depends: libbap
Priority: optional
Version: $BAP_VERSION
Description: Binary Analysis Platform C Library
EOF

cat > $DEBIAN/postinst <<EOF
#!/bin/sh
ln -sf $PREFIX/lib/libbap.so.$BAP_VERSION $PREFIX/lib/libbap.so
sudo ldconfig
EOF

cat > $DEBIAN/postrm <<EOF
#!/bin/sh
sudo ldconfig
EOF

chmod a+x $DEBIAN/postinst
chmod a+x $DEBIAN/postrm
sudo chown -R root:root bap/libbap-dev_$BAP_VERSION
dpkg-deb --build bap/libbap-dev_$BAP_VERSION

cd bap/
curl -L $SIGURL/sigs.zip > sigs.zip
curl -L $SIGURL/sigs.tar.gz > sigs.tar.gz

for pkg in bap libbap libbap-dev; do
    deb=$pkg\_$BAP_VERSION
    dir=$pkg-$BAP_VERSION
    sudo alien --to-rpm -g $deb.deb
    cd $dir
    spec=$(mktemp)
    awk '/%dir.*bap/ {print} /%dir/ {next} {print}' $pkg-$BAP_VERSION-2.spec > $spec
    sudo cp $spec $pkg-$BAP_VERSION-2.spec
    sudo rpmbuild -bb $pkg-$BAP_VERSION-2.spec --buildroot=$(pwd)
    echo "trying to run alien"
    echo alien --to-tgz $deb.deb
    cd ..
    ls -l
    alien --to-tgz $deb.deb
    sudo rm -rf $dir
done
