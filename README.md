# Overview
[![Build Status](https://travis-ci.org/BinaryAnalysisPlatform/bap.svg?branch=master)](https://travis-ci.org/BinaryAnalysisPlatform/bap)

`Bap` library provides basic facilities for doing binary analysis
in OCaml.

# Install

## Installing dependencies

The easiest way is to install packages using `opam` package manager:

```
 $ opam install core_kernel zarith
```

If you would like to use our serialization library, then please also
install a `piqi` tool with the following invocation:

```
 $ opam install piqi
```

If you're using a development version, i.e., just cloned us from github,
then you will also need an `oasis` tool, in order to create a build
environment.

```
 $ opam install oasis
```

## Compiling and installing `bap_core`

After all dependencies are installed, we can start the actual
build. In a development version you need to start with an `oasis
setup` command. If you're building a released sources, then you can
skip this section. Now, run the following triplet:

```
 $ ./configure --prefix=$(opam config var prefix)
 $ make
 $ make install
```

`./configure` script will check that everything is OK, and will finish
with errors if something goes wrong. So make sure, that you have
checked its result.

If you're already installed `bap_types` previously, then use

```
 $ make reinstall
```

command instead of `make install`. If it still complains, then try to
remove old `bap_types` manually with a `ocamlfind remove bap_types`
command.

# Using

## Using from top-level

It is a good idea to learn the library by playing in a top-level. If
you have `utop` installed, then you can just use our fancy `baptop`
script:

```
 $ baptop
 utop # open Bap.Std;;
```

Now, you can play with `Bap`. For example:

```
 utop # let x = Word.of_int32 0xDEADBEEFl;;
 val x : word = 0xDEADBEEF:32
 utop # let y = Word.of_int32 0xEFBEADDEl;;
 val y : word = 0xEFBEADDE:32
 let z = Word.Int.(!$x + !$y);;
 val z : Word.Int.t = Core_kernel.Result.Ok 0xCE6C6CCD:32
 utop # let z = Word.Int_exn.(x + y);;
 val z : word = 0xCE6C6CCD:32
 utop # Word.to_bytes x BigEndian |> Sequence.to_list;;
 - : word list = [0xDE:8; 0xAD:8; 0xBE:8; 0xEF:8]
```

If you do not want to use `baptop` or `utop`, then you can just type
the following in any ocaml top-level:

```
 # #use "topfind";;
 # #require "bap_core.top";;
 # open Bap.Std;;
```

And everything should work just out of box, i.e. it will load all
dependencies, install top-level printers, etc.


## Compiling with `bap_core`

As with top-level, you can use our script named `bapbuild` to start
working without tackling with all this build systems. Just type

```
  $ bapbuild mycoolprog.native
```

and everything should work just out-of-box. If `bapbuild` complains
that he can't find something, then make sure that you didn't skip
[Install] phase.

When using your own build environment, then make sure, that you have
added us as a dependency. We are installing as `ocamlfind`-enabled
project named `bap_core`. For example, if you're using `oasis`,
then just add `bap_core` to `BuildDepends` field. If you're using,
ocamlbuild with ocamlfind plugin, then add `package(bap_core)` or
`pkg_bap_core` to your `_tags` file.

## Learning Bap

TBD


# Development

TBD


# License
Please see the LICENSE file for licensing information.

# TODO

TBD
