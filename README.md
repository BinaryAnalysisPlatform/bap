# Overview

[![Join the chat at https://gitter.im/BinaryAnalysisPlatform/bap](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/BinaryAnalysisPlatform/bap?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Build Status](https://travis-ci.org/BinaryAnalysisPlatform/bap.svg?branch=master)](https://travis-ci.org/BinaryAnalysisPlatform/bap) [![docs](https://img.shields.io/badge/doc-v0.9.7-green.svg)](http://binaryanalysisplatform.github.io/bap/api/v0.9.7/Bap.Std.html) [![docs](https://img.shields.io/badge/doc-master-green.svg)](http://binaryanalysisplatform.github.io/bap/api/master/Bap.Std.html)

BAP is a platform for binary analysis. It is written in OCaml, but can
be used from other languages, for example, from Python.

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
And if you're interested in python bindings, then you can install them using pip:

```bash
$ pip install git+git://github.com/BinaryAnalysisPlatform/bap.git
```

# Usage

## Using from OCaml

There're two ways to use BAP. Compile your own application, and use
BAP library, or write a plugin, that can still use the library, but
will also get an access to decompiled binary. For the latter, write
your plugin in OCaml using your
[favorite text editor](https://github.com/BinaryAnalysisPlatform/bap/wiki/Emacs)
:
```sh
$ cat mycode.ml
open Bap.Std
let main project = print_endline "Hello, World"
let () = Project.register_plugin' main
```

Next, build it with our `bapbuild` tool:

```sh
$ bapbuild mycode.plugin
```

After this you can load your plugin with `-l` command line option, and
get an immediate access to the decompiled binary:

```sh
$ bap /bin/ls -lmycode
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

Now, you can play with BAP. For example:

```ocaml
utop # open Bap.Std;;
utop # let d = disassemble_file "/bin/ls";;
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

After BAP and python bindings are properly installed, you can start to
use it:

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

Bap is shipped with `bap` utility that can disassemble files, and
printout dumps in different formats, including plain text, json, dot,
html. The example of `bap` output is:

```asm
  begin(to_uchar)
      0000a004: 04 b0 2d e5    str r11, [sp, #-4]! ; STR_PRE_IMM(SP,R11,SP,0xfffffffc,0xe,Nil)
      0000a008: 00 b0 8d e2    add r11, sp, #0x0   ; ADDri(R11,SP,0x0,0xe,Nil,Nil)
      0000a00c: 0c d0 4d e2    sub sp, sp, #0xc    ; SUBri(SP,SP,0xc,0xe,Nil,Nil)
      0000a010: 00 30 a0 e1    mov r3, r0          ; MOVr(R3,R0,0xe,Nil,Nil)
      0000a014: 05 30 4b e5    strb r3, [r11, #-5] ; STRBi12(R3,R11,0xfffffffb,0xe,Nil)
      0000a018: 05 30 5b e5    ldrb r3, [r11, #-5] ; LDRBi12(R3,R11,0xfffffffb,0xe,Nil)
      0000a01c: 03 00 a0 e1    mov r0, r3          ; MOVr(R0,R3,0xe,Nil,Nil)
      0000a020: 00 d0 8b e2    add sp, r11, #0x0   ; ADDri(SP,R11,0x0,0xe,Nil,Nil)
      0000a024: 00 08 bd e8    ldm sp!, {r11}      ; LDMIA_UPD(SP,SP,0xe,Nil,R11)
      0000a028: 1e ff 2f e1    bx lr               ; BX_RET(0xe,Nil)
  end(to_uchar)
```

```ocaml
  begin(to_uchar) {
    mem := mem with [SP - 0x4:32, el]:u32 <- R11
    SP := SP - 0x4:32
    R11 := SP
    SP := SP - 0xC:32
    R3 := R0
    mem := mem with [R11 - 0x5:32, el]:u8 <- low:8[R3]
    temp_2303 := mem[R11 - 0x5:32, el]:u8
    R3 := pad:32[temp_2303]
    R0 := R3
    SP := R11
    orig_base_2309 := SP
    R11 := mem[orig_base_2309, el]:u32
    SP := SP + 0x4:32
    jmp LR
  }
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
