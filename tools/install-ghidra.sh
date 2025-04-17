# usage: install-ghidra.sh [dst [buildir]]
PREFIX=$(realpath ${1:-/usr})
BUILDDIR=${2:-$(mktemp -d)}
PROCESSORS=*
GHIDRA_VERSION=10.0.4
GHIDRA_BASENAME=Ghidra_${GHIDRA_VERSION}_build
GHIDRA_TARBALL=${GHIDRA_BASENAME}.tar.gz
GHIDRA_BASE_URL=https://github.com/NationalSecurityAgency/ghidra/archive/refs/tags
GHIDRA_SRC_URL=${GHIDRA_BASE_URL}/${GHIDRA_TARBALL}
GHIDRA_SHA256=76f1bd9604d49a2e59c47b347aedb90484bcfe51217c792e8cc771fc6481147c
GHIDRA_ROOT=ghidra-${GHIDRA_BASENAME}
LIBGHIDRA_PATH=${GHIDRA_ROOT}/Ghidra/Features/Decompiler/src/decompile/cpp
LIBGHIDRA_FLAGS="-O2 -std=c++11 -fPIC"
LIBGHIDRA_HEADERS_INSTALL_DIR=${PREFIX}/include/ghidra
LIBGHIDRA_LIBRARY_INSTALL_DIR=${PREFIX}/lib/ghidra
LIBGHIDRA_PROCESSORS_INSTALL_DIR=${PREFIX}/share/ghidra/Ghidra/Processors


set -x \
&& cd ${BUILDDIR} \
&& curl -sS -L -o ${GHIDRA_TARBALL} ${GHIDRA_SRC_URL} \
&& echo "${GHIDRA_SHA256} ${GHIDRA_TARBALL}" | sha256sum --check \
&& tar xzvf ${GHIDRA_TARBALL} \
&& rm ${LIBGHIDRA_PATH}/*bfd*.* ${LIBGHIDRA_PATH}/codedata.* \
&& make -j -C ${LIBGHIDRA_PATH} OPT_CXXFLAGS="${LIBGHIDRA_FLAGS}" libdecomp.a libsla.a \
&& make -j -C ${LIBGHIDRA_PATH} sleigh_opt \
&& ${LIBGHIDRA_PATH}/sleigh_opt -a ${GHIDRA_ROOT}/Ghidra \
&& install -d ${LIBGHIDRA_HEADERS_INSTALL_DIR} \
&& install -d ${LIBGHIDRA_LIBRARY_INSTALL_DIR} \
&& install -d ${LIBGHIDRA_PROCESSORS_INSTALL_DIR} \
&& install -t ${LIBGHIDRA_HEADERS_INSTALL_DIR} ${LIBGHIDRA_PATH}/*.h* \
&& install -t ${LIBGHIDRA_LIBRARY_INSTALL_DIR} ${LIBGHIDRA_PATH}/*.a \
&& cp -R ${GHIDRA_ROOT}/Ghidra/Processors/${PROCESSORS} ${LIBGHIDRA_PROCESSORS_INSTALL_DIR} \
&& cd - && rm -rf ${OLDPWD}
