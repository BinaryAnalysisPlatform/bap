#!/bin/sh

set -e

GITHUB=https://github.com/BinaryAnalysisPlatform/
SOURCE=$GITHUB/bap
BINDINGS=/home/ivg/factory/bap-bindings
BINARIES="bap bap-mc bapbundle bap-byteweight"
PREFIX=/usr/local
VERSION=1.1.0
OCAML=4.02.3
ARCH=$(dpkg-architecture -qDEB_BUILD_ARCH)

CONFDIR=$PREFIX/etc/bap
SWITCH=$(date +%s)
OLDSWITCH=$(opam config var switch)
SIGURL=$GITHUB/bap/releases/download/v$VERSION/sigs.zip

TMPDIR=$(mktemp -d)

echo "Getting fresh OCaml compiler"
sudo apt-get install opam

opam switch -A$OCAML $SWITCH

eval `opam config env`

echo "Looking in the dev-repo for the current list of dependencies"
opam pin add bap --dev-repo --yes -n
echo "Installing bap system dependencies"
opam install depext --yes
opam depext bap --yes
echo "Installing OCaml dependenices from OPAM"
opam install --deps-only bap --yes
echo "Done. Cleaning up..."
opam pin remove bap
echo "Clone a fresh repo"

[ -d bap-repo ] || git clone $SOURCE bap-repo
echo "Installing ocamlfind to the system path"

[ -f $PREFIX/bin/ocamlfind ] && cp $PREFIX/bin/ocamlfind $TMPDIR
sudo cp $(which ocamlfind) $PREFIX/bin


cd bap-repo
git pull
./configure --enable-everything \
            --disable-ida --disable-fsi-benchmark \
            --with-llvm-version=3.4 \
            --libdir=$(opam config var lib) \
            --plugindir=$PREFIX/lib/bap \
            --prefix=$PREFIX \
            --sysconfdir=$CONFDIR

make
sudo make reinstall
cd ..

echo "Packing a bap debian package"

sudo rm -rf bap

mkdir -p bap/bap_$VERSION/DEBIAN

BINDIR=bap/bap_$VERSION/$PREFIX/bin
LIBDIR=bap/bap_$VERSION/$PREFIX/lib/bap
SIGDIR=bap/bap_$VERSION/$PREFIX/share/bap
DEBIAN=bap/bap_$VERSION/DEBIAN

mkdir -p $BINDIR $LIBDIR $SIGDIR $DEBIAN

for binary in $BINARIES; do
    cp $PREFIX/bin/$binary bap/bap_$VERSION/$PREFIX/bin
done;

cp $PREFIX/lib/bap/*.plugin $LIBDIR
curl -L $SIGURL > $SIGDIR/sigs.zip

cat > $DEBIAN/control <<EOF
Package: bap
Architecture: $ARCH
Maintainer: Ivan Gotovchits
Depends: libgmp10, zlib1g, libstdc++6
Priority: optional
Version: $VERSION
Description: Binary Analysis Platform
EOF

sudo chown -R root:root bap/bap_$VERSION
dpkg-deb --build bap/bap_$VERSION

echo "now building the bindings"

[ -d bap-bindings ] || git clone $BINDINGS bap-bindings

cd bap-bindings
git pull
autoconf
./configure
make
cd ..


echo "Now packing libbap.deb"
LIBDIR=bap/libbap_$VERSION/$PREFIX/lib
DEBIAN=bap/libbap_$VERSION/DEBIAN

mkdir -p $LIBDIR $DEBIAN


cp bap-bindings/_build/bap/libbap.so $LIBDIR/libbap.so.$VERSION


cat > $DEBIAN/control <<EOF
Package: libbap
Architecture: $ARCH
Maintainer: Ivan Gotovchits
Depends: libgmp10, zlib1g, libstdc++6, libffi6
Priority: optional
Version: $VERSION
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
sudo chown -R root:root bap/libbap_$VERSION
dpkg-deb --build bap/libbap_$VERSION

echo "Now packing libbap-dev"
HDRDIR=bap/libbap-dev_$VERSION/$PREFIX/include
DEBIAN=bap/libbap-dev_$VERSION/DEBIAN

mkdir -p $DEBIAN $HDRDIR
cp bap-bindings/_build/bap/generated/bap.h $HDRDIR

cat > $DEBIAN/control <<EOF
Package: libbap-dev
Architecture: $ARCH
Maintainer: Ivan Gotovchits
Depends: libbap
Priority: optional
Version: $VERSION
Description: Binary Analysis Platform C Library
EOF

cat > $DEBIAN/postinst <<EOF
#!/bin/sh
ln -sf $PREFIX/lib/libbap.so.$VERSION $PREFIX/lib/libbap.so
sudo ldconfig
EOF

cat > $DEBIAN/postrm <<EOF
#!/bin/sh
sudo ldconfig
EOF

chmod a+x $DEBIAN/postinst
chmod a+x $DEBIAN/postrm
sudo chown -R root:root bap/libbap-dev_$VERSION
dpkg-deb --build bap/libbap-dev_$VERSION

echo "time to clean up..."
cd bap-repo
sudo make uninstall
cd ..
opam switch $OLDSWITCH
opam switch remove $SWITCH
