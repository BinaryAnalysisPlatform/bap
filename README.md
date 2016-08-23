# Overview

[![Join the chat at https://gitter.im/BinaryAnalysisPlatform/bap](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/BinaryAnalysisPlatform/bap?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Build Status](https://travis-ci.org/BinaryAnalysisPlatform/bap.svg?branch=master)](https://travis-ci.org/BinaryAnalysisPlatform/bap) [![docs](https://img.shields.io/badge/doc-v0.9.9-green.svg)](http://binaryanalysisplatform.github.io/bap/api/v0.9.9/Bap.Std.html) [![docs](https://img.shields.io/badge/doc-master-green.svg)](http://binaryanalysisplatform.github.io/bap/api/master/Bap.Std.html)

BAP is a platform for binary analysis. It is written in OCaml, but can
be used from other languages.

# <a name="Installation"></a>Installation

BAP is released using `opam` package manager. After you've successfully
[installed](https://opam.ocaml.org/doc/Install.html) opam, do the following:

```bash
$ opam init --comp=4.02.1
$ eval `opam config env`
$ opam install depext
$ opam depext bap
$ opam install bap
```

# Usage

## Using from OCaml

There're two ways to use BAP: compile your own stand-alone
application, and use BAP library, or write a plugin, that can still
use the library, but will also get an access to decompiled binary, as
well as intergration with tools and other plugins. For the latter,
write your plugin in OCaml using your
[favorite text editor](https://github.com/BinaryAnalysisPlatform/bap/wiki/Emacs)
:

```sh
$ cat hello_world.ml
open Bap.Std
let main project = print_endline "Hello, World"
let () = Project.register_pass' "hello-world" main
```

Next, build it with our `bapbuild` tool:

```sh
$ bapbuild hello_world.plugin
```

After this you can load your plugin with `-l` command line option, and
get an immediate access to the decompiled binary:

```sh
$ bap /bin/ls -lhello-world
```

`bapbuild` can compile a standalone applications, not only plugins. In
fact, `bapbuild` underneath the hood is an `ocamlbuild` utility extended
with our rules an flags. To compile a standalone binary,

```bash
$ bapbuild mycoolprog.native
```

If `bapbuild` complains that something is missing, make sure that you
didn't skip the [Installation](#Installation) phase. You can add your
own dependencies with a `-pkg` or `-pkgs` command line options:

```bash
$ bapbuild -pkg lwt mycoolprog.native
```

If you use your own build environment, please make sure that you have
added `bap` as a dependency. We install our libraries using
`ocamlfind` and you just need to add `bap` to your project. For
example, if you use `oasis`, then you should add `bap` to the
`BuildDepends` field. If you are using `ocamlbuild` with the
`ocamlfind` plugin, then you should add `package(bap)` or `pkg_bap` to
your `_tags` file.


## Using from top-level

It maybe a good idea to learn how to use our library by playing in an
OCaml top-level. If you have installed `utop`, then you can just use
our `baptop` script to run `utop` with `bap` extensions:

```bash
$ baptop
```

Now, you can play with BAP. The following example, will create a
project from
[coreutils_O2_true](https://github.com/BinaryAnalysisPlatform/arm-binaries/raw/master/coreutils/coreutils_O2_true)
file, build callgraph of a program, control flow graph and dominance
tree of a `main` function.

```ocaml
utop # open Core_kernel.Std;;
utop # open Bap.Std;;
utop # let proj = Project.from_file "coreutils_O2_true" |> ok_exn;;
utop # let prog = Project.program proj;;
utop # let cg = Program.to_graph prog;;
utop # let main = Term.find_exn sub_t prog Tid.(!"@main");;
utop # let cfg = Sub.to_cfg main;;
utop # module G = Graphlib.Ir;;
utop # let entry = Option.value_exn (Term.first blk_t main);;
utop # let dom_tree = Graphlib.dominators (module G) cfg (G.Node.create entry);;
```

Note: if you do not want to use `baptop` or `utop`, then you can
execute the following in any OCaml top-level:

```ocaml
# #use "topfind";;
# #require "bap.top";;
# open Bap.Std;;
```

And everything should work just out of box, i.e. it will load all the
dependencies, install top-level printers, etc.


## Using from shell

Bap is shipped with `bap` utility that can disassemble files, and
printout dumps in different formats, including plain text, json, dot,
html. The example of `bap` output is:

```
00000088: sub strcpy(arg_0, arg_1)
00000151: arg_0 :: u32 = R0
00000152: arg_1 :: u32 = R1
0000005f:
00000063: ZF.1 := R0 = 0x0:32
00000064: when ZF.1 return LR
00000065: goto %00000066

00000066:
00000067: t_614.1 := mem[R1, el]:u8
00000068: R3.1 := pad:32[t_614.1]
0000006c: ZF.2 := R3.1 = 0x0:32
0000006d: when ZF.2 goto %0000006f
0000006e: goto %00000076

0000006f:
00000070: R12.1 := R0
00000071: goto %00000072

00000072:
0000012f: R1.1 := phi([R1, %0000006f], [R1.3, %00000086])
00000131: R12.2 := phi([R12.1, %0000006f], [R12.4, %00000086])
00000133: R2.1 := phi([R2, %0000006f], [R2.4, %00000086])
00000135: R3.2 := phi([R3.1, %0000006f], [R3.5, %00000086])
00000137: mem.1 := phi([mem, %0000006f], [mem.4, %00000086])
00000073: R3.3 := 0x0:32
00000074: mem.2 := mem.1 with [R12.2, el]:u8 <- low:8[R3.3]
00000075: return LR
...
```

Also we're shipping a `bap-mc` executable that can disassemble
arbitrary strings and output them in a plethora of formats. Read
`bap-mc --help` for more information. `bap-byteweight` utility can be
used to evaluate our `byteweight` algorithm for finding symbols inside
the binary. It is also a supporting toolkit for byteweight
infrastructure, it can download, create and install binary signatures,
used for identification.


## Using from other languages

BAP exposes most of its functionality using `JSON`-based RPC protocol,
specified
[Public API Draft](https://github.com/BinaryAnalysisPlatform/bap/wiki/Public-API-%5Bdraft%5D)
doument. The protocol is implemented by `bap-server` program that is
shipped with bap by default. You can talk with server using `HTTP`
protocol, or extend it with any other transporting protocol you would
like.

## Extending BAP

We're always welcome for any contributions. If you want to add new
code, or fix a bug, feel free to clone us, and create a pull request.

But BAP can also be extended in a non invasive way, using plugin
system. That means, that you can use `bap` library, to extend the
`bap` library! See our
[blog](http://binaryanalysisplatform.github.io/bap_plugins/) for more
information.

## Learning BAP

Other than [API](http://binaryanalysisplatform.github.io/bap/api/v0.9.7/Bap.Std.html) documentation, we have [blog](http://binaryanalysisplatform.github.io/bap_plugins/) and
[wiki](https://github.com/BinaryAnalysisPlatform/bap/wiki/), where you
can find some useful information. Also, we have a permanently manned
chat in case of emergency. Look at the badge on top of the README file,
and feel free to join.

# License

Please see the `LICENSE` file for licensing information.
