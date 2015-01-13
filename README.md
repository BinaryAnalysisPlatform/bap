# Overview

[![Build Status](https://travis-ci.org/BinaryAnalysisPlatform/bap.svg?branch=master)](https://travis-ci.org/BinaryAnalysisPlatform/bap)

`Bap` library provides basic facilities for performing binary analysis
in OCaml and other languages.

# <a name="Installation"></a>Installation

## Installing `bap` dependencies

### Installing system dependencies

There are few system libraries that bap depends on. We provide a file
`apt.deps` that contains package names as they are in Ubuntu
Trusty. Depending on your OS and distribution, you may need to adjust
this names. But, on most Debian-based Linux distribution, this should work:

```bash
$ sudo apt-get install $(cat apt.deps)
```

### Installing OCaml dependencies

The easiest way to install the OCaml dependencies of `bap` is to use
the `opam` package manager:

```bash
$ opam install $(cat opam.deps)
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

## Compiling and installing `bap`

Once all the dependencies of `bap` have been installed, we can start the actual
build. Now, run the following commands:

```bash
$ make
$ make install
```

This will run take care to run all configuration scripts for you. If
you want to provide some specific flags to `configure`, then you need
either to invoke it manually with `./configure` or provide them to
make using `BAPCONFIGUREFLAGS` environment variable.

Note: if you have chosen prefix that require super-user privileges,
then you need to run `make install` using either `sudo`, e.g., `sudo
make install` or switch to a super-user mode. Although it is not
required, we suggest to install `bap` in to `opam` stack. In this case
a proper prefix can be generated using `opam config var` command,
e.g.,

```bash
./configure --prefix=$(opam config var prefix)
```

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

## Using from Python

You can install `bap` python bindings with `pip`.

```bash
$ pip install bap/python
```

Where `bap/python` is a path to bap python bindings. Adjust it
according to your setup. Also, you may need to use `sudo` or to
activate your `virtualenv` if you're using one.

If you don't like `pip`, then you can just go to `bap/python` folder
and copy-paste the contents to whatever place you like, and use it as
desired.

After bindings are properly installed, you can start to use it:

```python
    >>> import bap
    >>> print '\n'.join(insn.asm for insn in bap.disasm("\x48\x83\xec\x08"))
        decl    %eax
        subl    $0x8, %esp
```

A more complex example:

```python
    >>> img = bap.image('coreutils_O0_ls')
    >>> sym = img.get_symbol('main')
    >>> print '\n'.join(insn.asm for insn in bap.disasm(sym))
        push    {r11, lr}
        add     r11, sp, #0x4
        sub     sp, sp, #0xc8
        ... <snip> ...
```

For more information, read builtin documentation, for example with
`ipython`:

```python
    >>> bap?
```

## Using from shell

We're shipping a `bap-mc` executable that can disassemble arbitrary
strings. Read `bap-mc --help` for more information.

## Using from other languages

BAP exposes most of its functionality using `JSON`-based RPC protocol,
specified
[Public API Draft](https://github.com/BinaryAnalysisPlatform/bap/wiki/Public-API-%5Bdraft%5D)
doument. The protocol is implemented by `bap-server` program that is
shipped with bap by default. You can talk with server using `HTTP`
protocol, or extend it with any other transporting protocol you would
like.


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

## Extending BAP

BAP can be extended using plugin system. That means, that you can use
`bap` library, to extend the `bap` library! See our
[blog](http://binaryanalysisplatform.github.io/bap_plugins/) for more
information.


## Learning BAP

The best source of information about BAP is it's source code, that is
well-documented. There are also
[blog](http://binaryanalysisplatform.github.io/bap_plugins/) and
[wiki](https://github.com/BinaryAnalysisPlatform/bap/wiki/), where you
can find some useful information.

# License

Please see the `LICENSE` file for licensing information.
