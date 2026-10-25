{ lib
, stdenv
, ocamlPackages
, dune_3
, llvmPackages
, zlib
, gmp
, libffi
, openssl
, ncurses
, readline
, libxml2
, sqlite
, z3
, curl
, pcre
, pcre2
, zstd
, pkg-config
, git
, src
}:

let
  llvm = llvmPackages.llvm;

  # The fork carries the OCaml libraries that Nixpkgs does not package
  # (bitvec, graphlib, monads, ogre, regular, text-tags, ppx_bap) in its
  # own workspace; everything else comes from Nixpkgs.
  ocamlDeps = with ocamlPackages; [
    findlib
    dune-site
    dune-configurator
    dune-private-libs

    base
    stdio
    core_kernel
    bin_prot
    sexplib
    sexplib0
    parsexp
    stdlib-shims
    ocamlgraph
    yojson
    ezjsonm

    ppxlib
    ppx_assert
    ppx_bench
    ppx_bin_prot
    ppx_compare
    ppx_cold
    ppx_enumerate
    ppx_expect
    ppx_fields_conv
    ppx_hash
    ppx_here
    ppx_inline_test
    ppx_optcomp
    ppx_sexp_conv
    ppx_sexp_value
    ppx_variants_conv
    ppx_bitstring
    bitstring

    cmdliner_1
    re
    uri
    uuidm
    xmlm
    zarith
    z3
    camlzip
    ocurl
    linenoise
    mmap
    fileutils
    ocamlbuild
    frontc
  ];
in
stdenv.mkDerivation {
  pname = "bap";
  version = "2.6.0-alpha";

  inherit src;

  nativeBuildInputs = [
    ocamlPackages.ocaml
    ocamlPackages.findlib
    dune_3
    pkg-config
    git
    llvm.dev
  ];

  # The checked-in `configure` is the old oasis driver; dune generates
  # config.status from config.status.in instead.
  dontConfigure = true;

  buildInputs = ocamlDeps ++ [
    llvm.lib
    z3
    pcre
    zstd
    ncurses
    readline
    gmp
    zlib
    openssl
    libffi
    curl
    libxml2
    sqlite
  ];

  # BAP's configure is a dune rule over config.status.in.  Writing the
  # file in the source tree overrides the empty fallback and bakes the
  # final store prefix into the installed configuration.
  postPatch = ''
    # BAP's LLVM configurator hardcodes the legacy -lcurses link name;
    # Nixpkgs' ncurses only installs -lncurses.
    substituteInPlace lib/bap_llvm/config/llvm_configurator.ml \
      --replace-fail '"-lcurses"' '"-lncurses"'

    # Dune expands the site pforms against its default install root
    # (_build/install) on a plain build.  Point the generated site
    # tables at the final store prefix instead.
    substituteInPlace plugins/primus_lisp/primus_lisp_config.ml.ab \
      --replace-fail '$'{site_lisp} "\"$out/share/bap-common/lisp\"" \
      --replace-fail '$'{site_site_lisp} "\"$out/share/bap-common/site_lisp\"" \
      --replace-fail '$'{site_semantics} "\"$out/share/bap-common/semantics\""
    substituteInPlace plugins/api/api_sites.ml.ab \
      --replace-fail '$'{site_api} "\"$out/share/bap-common/api\""

    cat > config.status.in <<EOF
    --prefix=${placeholder "out"}
    --with-llvm-config=${llvm.dev}/bin/llvm-config
    EOF
  '';

  dontDetectOcamlConflicts = true;

  passthru = { inherit ocamlDeps; };

  buildPhase = ''
    runHook preBuild
    dune build @install --profile release --display short
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    dune install --prefix "$out" --profile release --display short
    runHook postInstall
  '';

  meta = with lib; {
    description = "Binary Analysis Platform (Angs12 fork)";
    homepage = "https://github.com/Angs12/bap";
    license = licenses.mit;
    platforms = platforms.linux;
  };
}
