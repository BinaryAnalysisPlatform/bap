# Overview

[![Build Status](https://travis-ci.org/BinaryAnalysisPlatform/bap.svg?branch=master)](https://travis-ci.org/BinaryAnalysisPlatform/bap)

`Bap` library provides basic facilities for performing binary analysis
in OCaml and other languages.

# <a name="Installation"></a>Installation

BAP is released using `opam` package manager. After you've successfully
[installed](https://opam.ocaml.org/doc/Install.html) opam, and have all
[system dependencies](#sysdeps) satisfied, you can proceed with:

```bash
$ opam install bap
```

And if you're interested in python bindings, then you can install them using pip:

```bash
$ pip install git+git://github.com/BinaryAnalysisPlatform/bap.git
```

## Installing `bap` dependencies

### <a name="sysdeps"></a>Installing system dependencies

There are few system libraries that bap depends on, namely `llvm-3.4` and `clang` compiler.
We provide a file `apt.deps` that contains package names as they are in Ubuntu
Trusty. Depending on your OS and distribution, you may need to adjust
this names. But, on most Debian-based Linux distribution, this should work:

```bash
$ sudo apt-get install $(cat apt.deps)
```


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
utop # let d = disassemble_file "ls";;
val d : t = <abstr>
utop # let insn = Disasm.insn_at_addr d (Addr.of_int32 0xa9dbl);;
val insn : (mem * insn) option = Some (0000a9d8: 01 00 00 0a , beq #0x4; Bcc(0x4,0x0,CPSR))
let blk = Disasm.blocks d |> Table.elements |> Seq.hd_exn;;
val blk : block = [991c, 9923]
utop # Block.leader blk;;
- : insn = push {r3, lr}; STMDB_UPD(SP,SP,0xe,Nil,R3,LR)
utop # Block.terminator blk |> Insn.bil;;
- : Bap_types.Std.bil = [LR = 0x9924:32; jmp 0x9ED4:32]
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
$ pip install git+git://github.com/BinaryAnalysisPlatform/bap.git
```


Instead of git path you can also use a local one. Adjust it according
to your setup. Also, you may need to use `sudo` or to activate your
`virtualenv` if you're using one.

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

Bap is shipped with `bap-objdump` utility that can disassemble files,
and printout dumps in different formats, including plain text, json,
dot, html. The example of `bap-objdump` output is:

```ocaml
  begin(sub_B6C8_0xec)
      0000b7b4: 00 00 50 e3    cmp r0, #0x0        ; CMPri(R0,0x0,0xe,Nil)
      0000b7b8: e4 ff ff ca    bgt #-0x70          ; Bcc(-0x70,0xc,CPSR)
  end(sub_B6C8_0xec)

  begin(sub_B6C8_0xec) {
    orig1_2673 = R0
    orig2_2674 = 0x0:32
    dest_2671 = R0 - 0x0:32
    CF = orig2_2674 <= orig1_2673
    VF = high:1[(orig1_2673 ^ orig2_2674) & (orig1_2673 ^ dest_2671)]
    NF = high:1[dest_2671]
    ZF = dest_2671 = 0x0:32
    if ((ZF = false) & (NF = VF)) {
      jmp sub_B6C8_0x88
    }
  }
```

Also we're shipping a `bap-mc` executable that can disassemble arbitrary
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

If you have other dependencies, you can compile it using `pkg` flag, like this

```bash
$ bapbuild -pkg lwt mycoolprog.native
```

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
