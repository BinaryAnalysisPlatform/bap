#!/bin/sh

set -e



# use vargrant to build packages
export VERSION=1.2.0

# This script will create an rpm file, and requires sudo to operate.
#
# It is a hot for the alien bug, that prevents alien-generated rpms to
# work on modern hats, e.g., fedora 20+, centos 7, etc. The problem is
# that alien generates directories that belong to the filesystem
# package, e.g., `/`, `/usr`, etc.. In this fix we still use alien,
# but we patch the spec with an awk command, that will remove all
# directories, that do no contain `bap` in it.
buildrpm() {
    deb=$(basename -s.deb $1)
    pkg=$(echo $deb | cut -d_ -f1)
    ver=$(echo $deb | cut -d_ -f2)
    dir=$pkg-$ver

    sudo apt-get install alien
    sudo rm -rf $dir
    sudo alien --to-rpm -g $deb.deb
    cd $dir
    spec=`mktemp`
    awk '/%dir.*bap/ {print} /%dir/ {next} {print}' $pkg-$ver-2.spec > $spec
    sudo cp $spec $pkg-$ver-2.spec
    sudo rpmbuild -bb $pkg-$ver-2.spec --buildroot=`pwd`
    cd ..
    sudo rm -rf $dir
}


# build system distribution llvm-version
build() {
    system=$1
    distro=$2
    llvm=$3
cat > Vagrantfile <<EOF
Vagrant.configure(2) do |config|
  config.vm.box = "$system/$distro"
  config.vm.provider "virtualbox" do |vb|
      vb.memory = "4096"
  end
  config.vm.provision "shell", privileged: false, inline: <<-SHELL
set -e
sudo apt-get update
sudo apt-get --yes install clang wget m4 curl autoconf dpkg-dev libgmp-dev libzip-dev libcurl4-gnutls-dev llvm-$llvm-dev time unzip git
export eval \`dpkg-architecture\`

echo "getting opam from https://github.com/ocaml/opam/releases/download/1.2.2/opam-1.2.2-\$DEB_HOST_GNU_CPU-Linux"

#https://github.com/ocaml/opam/releases/download/1.2.2/opam-1.2.2-x86_64-Linux
curl -L https://github.com/ocaml/opam/releases/download/1.2.2/opam-1.2.2-\$DEB_HOST_GNU_CPU-Linux > opam
echo "installing opam"
sudo chmod a+x opam
sudo cp opam /usr/local/bin
echo "staring packaging"
`cat deb-build.sh`
cp bap/*.deb /vagrant/
SHELL
end
EOF
vagrant destroy -f
vagrant up
buildrpm bap_$VERSION.deb
buildrpm libbap_$VERSION.deb
buildrpm libbap-dev_$VERSION.deb
alien --to-tgz bap_$VERSION.deb
alien --to-tgz libbap_$VERSION.deb
alien --to-tgz libbap-dev_$VERSION.deb
}

build ubuntu trusty64 3.4
