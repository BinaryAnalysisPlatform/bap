# Overview

[![Build Status](https://travis-ci.org/BinaryAnalysisPlatform/bap.svg?branch=master)](https://travis-ci.org/BinaryAnalysisPlatform/bap)

`Bap` library provides basic facilities for performing binary analysis in OCaml.

# <a name="Installation"></a>Installation

## Installing `bap` dependencies

The easiest way to install the OCaml dependencies of `bap` is to use
the `opam` package manager:

```bash
$ opam install bitstring core_kernel zarith
```

_Note:_ The most up-to-date source of our dependency list is in our travis
automation script `.travis-ci.sh`. The variable `SYS_DEPENDS` lists dependencies
that should be installed on your system using `apt-get`; the variable
`OPAM_DEPENDS` lists dependencies that can be installed via `opam`.

If you would like to use our serialization library, then please also install the
`piqi` package as follows:

```bash
$ opam install piqi
```

If you are using a development version, e.g., you have just cloned this from
github, then you will also need the `oasis` package in order to create a build
environment.

```bash
$ opam install oasis
```

We also recommend you install `utop` for running BAP.  

```bash
$ opam install utop
```

Finally, you need to now install LLVM.  LLVM often changes their APIs,
so we have had to standardize against one.  BAP currently compiles
against llvm-3.4, which we have confirmed works on OSX and Ubuntu.

## Compiling and installing `bap`

Once all the dependencies of `bap` have been installed, we can start the actual
build. In a development version, you need to start by executing `oasis setup`.
If you are building from a release, e.g., you have just downloaded a tarball,
then you can skip this step. Now, run the following commands:

```bash
$ oasis setup  #needed only if you have cloned from git
$ ./configure --prefix=$(opam config var prefix)
$ make
$ make install
```

The `./configure` script will check that everything is OK. If not, it will
terminate with error messages displayed on the console. Please be sure to check
the console output.

If you have installed `bap` previously, then use the command `make reinstall`
instead of `make install`. However, this will *not* work if `setup.log` has been
erased (by, for example, `git clean -fdx` or `make clean`). In that case, you
can remove the old `bap` installation manually via the command `ocamlfind remove
bap`.

# Usage

## Using from top-level

It is a good idea to learn how to use our library by playing in an OCaml
top-level. If you have installed `utop`, then you can just use our `baptop`
script to run `utop` with `bap` extensions:

```bash
$ baptop
```

Now, you can play with BAP. For example:

```ocaml
utop # open Bap.Std;;
utop # let x = Word.of_int32 0xDEADBEEFl;;
val x : word = 0xDEADBEEF:32
utop # let y = Word.of_int32 0xEFBEADDEl;;
val y : word = 0xEFBEADDE:32
utop # let z = Word.Int.(!$x + !$y);;
val z : Word.Int.t = Core_kernel.Result.Ok 0xCE6C6CCD:32
utop # let z = Word.Int_exn.(x + y);;
val z : word = 0xCE6C6CCD:32
utop # Word.to_bytes x BigEndian |> Sequence.to_list;;
- : word list = [0xDE:8; 0xAD:8; 0xBE:8; 0xEF:8]
```

If you do not want to use `baptop` or `utop`, then you can execute the following
in any OCaml top-level:

```ocaml
# #use "topfind";;
# #require "bap.top";;
# open Bap.Std;;
```

And everything should work just out of box, i.e. it will load all the
dependencies, install top-level printers, etc.

## Compiling your program with `bap`

Similar to the top-level, you can use our `bapbuild` script to compile a program
that uses `bap` without tackling with the build system. For example, if your
program is `mycoolprog.ml`, then you can execute:

```bash
$ bapbuild mycoolprog.native
```

and you will obtain `mycoolprog.native`. If `bapbuild` complains that something
is missing, make sure that you didn't skip the [Installation](#Installation)
phase. You can add your own dependencies with a `-package` command line option.

If you use your own build environment, please make sure that you have added
`bap` as a dependency. We install our libraries using `ocamlfind` and you just
need to add `bap` to your project. For example, if you use `oasis`, then you
should add `bap` to the `BuildDepends` field. If you are using `ocamlbuild` with
the `ocamlfind` plugin, then you should add `package(bap)` or `pkg_bap` to your
`_tags` file.

## Learning BAP

TBD

# Development

TBD

# License

Please see the `LICENSE` file for licensing information.

# TODO

TBD
