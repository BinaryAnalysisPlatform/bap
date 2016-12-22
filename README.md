# Overview

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/BinaryAnalysisPlatform/bap/blob/master/LICENSE)
[![Join the chat at https://gitter.im/BinaryAnalysisPlatform/bap](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/BinaryAnalysisPlatform/bap?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![docs](https://img.shields.io/badge/doc-1.1.0-green.svg)](http://binaryanalysisplatform.github.io/bap/api/v1.1.0/argot_index.html)
[![docs](https://img.shields.io/badge/doc-master-green.svg)](http://binaryanalysisplatform.github.io/bap/api/master/argot_index.html)
[![Build Status](https://travis-ci.org/BinaryAnalysisPlatform/bap.svg?branch=master)](https://travis-ci.org/BinaryAnalysisPlatform/bap)
[![pip](https://img.shields.io/badge/pip-1.1.0-green.svg)](https://pypi.python.org/pypi/bap/)

Binary Analysis Platform is a framework for writing program analysis
tools, that target binary files. The framework consists of a plethora
of libraries, plugins, and frontends. The libraries provide code
reusability, the plugins facilitate extensibility, and the frontends
serve as entry points.

# <a name="Installation"></a>Installation

We use the OPAM package manager to handle installation. After you've
successfully [installed](https://opam.ocaml.org/doc/Install.html)
OPAM, do the following:

```bash
opam init --comp=4.02.3    # install the compiler
opam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository
eval `opam config env`               # activate opam environment
opam depext --install bap            # install bap
```

Got any problems? Then visit our [troubleshooting page](https://github.com/BinaryAnalysisPlatform/bap/wiki/Troubleshooting-installation).

# Usage

## Shell

The BAP main frontend is a command line utility called `bap`. You can
use it to explore the binary, run existing analysis, plugin your own
behavior, load traces, and much more.

To dump a program in various formats use the `--dump` option (or its short
equivalent, `-d`), For example, let's run bap on
[arm-linux-gnueabi](https://github.com/BinaryAnalysisPlatform/bap-testsuite/blob/master/bin/arm-linux-gnueabi-echo)
file.

```fortran
$ bap arm-linux-gnueabi-echo -d | grep main -A16
000000ca: sub main(main_argc, main_argv, main_result)
00000164: main_argc :: in u32 = R0
00000165: main_argv :: in out u32 = R1
00000166: main_result :: out u32 = R0
00000050:
00000051: v618 := SP
00000052: mem := mem with [v618 - 0x4:32, el]:u32 <- LR
00000053: mem := mem with [v618 - 0x8:32, el]:u32 <- R11
00000054: mem := mem with [v618 - 0xC:32, el]:u32 <- R10
00000055: mem := mem with [v618 - 0x10:32, el]:u32 <- R8
00000056: mem := mem with [v618 - 0x14:32, el]:u32 <- R7
00000057: mem := mem with [v618 - 0x18:32, el]:u32 <- R6
00000058: mem := mem with [v618 - 0x1C:32, el]:u32 <- R5
00000059: mem := mem with [v618 - 0x20:32, el]:u32 <- R4
0000005a: SP := SP - 0x20:32
0000005b: R11 := SP + 0x1C:32
0000005c: SP := SP - 0x18:32
0000005d: mem := mem with [R11 - 0x30:32, el]:u32 <- R0
0000005e: mem := mem with [R11 - 0x34:32, el]:u32 <- R1
0000005f: R3 := SP
```

By default, the `--dump` options used the IR format, but you can
choose from various other formats. Use the `--list-formats` option to
get the list. However, formats are provided by plugins, so just
because you don't see your preferred format listed doesn't mean you
can't generate it. Check OPAM for plugins which may provide the format
you want to read (the bap-piqi plugin provides protobuf, xml, and
json, which cover many use cases).

To discover what plugins are currently available, use the
`--list-plugins` option. A short description will be printed for each
plugin. You can also use the `opam search bap` command, to get the
information about other bap packages.

To get information about a specific plugin named `<PLUGIN>` use
the `--<PLUGIN>-help` option, e.g., `bap --llvm-help`.

The `bap` utility works with whole binaries; if you have just few
bytes with which you would like to tackle, then `bap-mc` is what you
are looking for.


## OCaml

An idiomatic way of using BAP is to extend it with a plugin. Suppose,
you want to write some analysis. For example, let's estimate the ratio
of jump instructions to the total amount of instructions (a value that
probably correlates with a complexity of a program).

So, let's do it. Create an empty folder, then open your
[favorite text editor](https://github.com/BinaryAnalysisPlatform/bap/wiki/Emacs)
and write the following program in a `jmp.ml` file:

```ocaml
open Core_kernel.Std
open Bap.Std

let counter = object
  inherit [int * int] Term.visitor
  method! enter_term _ _ (jmps,total) = jmps,total+1
  method! enter_jmp _ (jmps,total) = jmps+1,total
end

let main proj =
  let jmps,total = counter#run (Project.program proj) (0,0) in
  printf "ratio = %d/%d = %g\n" jmps total (float jmps /. float total)


let () = Project.register_pass' main
```

Before we run it, let's go through the code. The `counter` object is a
visitor that has a state consisting of a pair of counters. The first
counter keeps track of the number of jmp terms, and the second counter
is incremented every time we enter any term.  The `main` function
just runs the counter. Finally, we register it with the
`Project.register_pass'` function. Later the function can be invoked
from a command line, and it will get a project data structure, that
contains all the information that was recovered from a binary.

To compile the plugin simply run the following command:

```
bapbuild jmp.plugin
```

It is easier to run the pass, if it is installed, so let's do it:

```
bapbundle install jmp.plugin
```

Now we can test it:
```
$ bap /bin/true --pass=jmp
ratio = 974/7514 = 0.129625
$ bap /bin/ls --pass=jmp
ratio = 8917/64557 = 0.138126
```

## Python

OK, If the previous example doesn't make any sense to you, then you
can try our
[Python bindings](https://github.com/BinaryAnalysisPlatform/bap-python).
Install them with `pip install bap` (you still need to install `bap`
beforehand). Here is the same example, but in Python:

```python
import bap
from bap.adt import Visitor

class Counter(Visitor) :
    def __init__(self):
        self.jmps = 0
        self.total = 0

    def enter_Jmp(self,jmp):
        self.jmps += 1

    def enter_Term(self,t):
        self.total += 1

proj = bap.run('/bin/true')
count = Counter()
count.run(proj.program)
print("ratio = {0}/{1} = {2}".format(count.jmps, count.total,
                                     count.jmps/float(count.total)))
```


## baptop

BAP also ships an interactive toplevel, aka REPL. This is a shell-like
program that will interactively evaluate OCaml instructions and
print the results. Just run:

```bash
$ baptop
```

Now, you can play with BAP. The following example will open a file,
build callgraph of a program, and a control flow graph with a
dominance tree of a function.

```ocaml
open Core_kernel.Std;;
open Bap.Std;;
open Graphlib.Std;;
let rooter = Rooter.Factory.find "byteweight" |> Option.value_exn;;
let proj = Project.create ~rooter (Project.Input.file "/bin/true") |> ok_exn;;
let prog = Project.program proj;;
let cg = Program.to_graph prog;;
let sub = Term.first sub_t prog |> Option.value_exn;;
let cfg = Sub.to_cfg sub;;
module G = Graphs.Ir;;
let entry = Option.value_exn (Term.first blk_t sub);;
let dom_tree = Graphlib.dominators (module G) cfg (G.Node.create entry);;
```

Note: if you do not want to use `baptop` or `utop`, then you can
execute the following in any OCaml top-level:

```ocaml
#use "topfind";;
#require "bap.top";;
```

## RPC

Some of BAP functionality is exposed via `JSON`-based RPC protocol,
specified
[Public API Draft](https://github.com/BinaryAnalysisPlatform/bap/wiki/Public-API-%5Bdraft%5D)
document. The protocol is implemented by `bap-server` program, that
can be installed with `opam install bap-server` command. You can talk
with server using `HTTP` protocol, or extend it with any other
transporting protocol you would like.


# Learning

Other than
[API](https://binaryanalysisplatform.github.io/bap/api/v1.0.0/argot_index.html)
documentation, we have
[blog](https://binaryanalysisplatform.github.io/) and
[wiki](https://github.com/BinaryAnalysisPlatform/bap/wiki/), where you
can find some useful information. Also, we have a permanently manned
chat in case of emergency. Look at the badge on top of the README
file, and feel free to join.

# Contributing

BAP is a framework, so you don't need to change its code to extend
it.We use the dependency injection principle with many injection
points to allow the user to alter BAP behavior. However, bugs happen,
so if you have any problems, questions or suggestions, please, don't
hesitate to use our issue tracker. Submitting a pull request with a
problem fix will make us really happy. However, we will only accepted
pull requests that have MIT license.

If you wrote analysis with BAP, then don't hesitate to
[release](https://opam.ocaml.org/doc/Packaging.html) it to OPAM, for
the benefit of the community.
