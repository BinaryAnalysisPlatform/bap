# Overview

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/BinaryAnalysisPlatform/bap/blob/master/LICENSE)
[![Join the chat at https://gitter.im/BinaryAnalysisPlatform/bap](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/BinaryAnalysisPlatform/bap?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![docs](https://img.shields.io/badge/doc-1.6.0-green.svg)](http://binaryanalysisplatform.github.io/bap/api/v1.6.0/argot_index.html)
[![docs](https://img.shields.io/badge/doc-master-green.svg)](http://binaryanalysisplatform.github.io/bap/api/master/index.html)
[![docs](https://img.shields.io/badge/odoc-master-green.svg)](http://binaryanalysisplatform.github.io/bap/api/odoc/index.html)
[![Build Status](https://travis-ci.org/BinaryAnalysisPlatform/bap.svg?branch=master)](https://travis-ci.org/BinaryAnalysisPlatform/bap)
[![pip](https://img.shields.io/badge/pip-1.3.0-green.svg)](https://pypi.python.org/pypi/bap/)

The Carnegie Mellon University Binary Analysis Platform (CMU BAP) is a suite of utilities and libraries that enables analysis of programs in their machine representation. BAP includes an evergrowing set of ready to use [tools][1] and provides various facilities for building custom tools, starting from various analysis-specific domain languages, such as, Primus Lisp, BML, BARE, Recipes, etc which do not require sophisticated programming skills and ending with implementing custom plugins in OCaml or even in Rust or C, via provided bindings.  The following short [demonstration][2] of BAP capabilities is interactive, you can pause it at any moment and even copy the contents. 


[1]: https://github.com/BinaryAnalysisPlatform/bap-toolkit
[2]: http://binaryanalysisplatform.github.io/assets/playfull.svg

BAP supports most of the common hardware architectures, like ARM, x86, x86-64, PowerPC, and MIPS. More architectures could be easily added using a 



The Carnegie Mellon University Binary Analysis Platform (CMU BAP) is a reverse engineering and program analysis platform
that works with binary code and doesn't require the source code. BAP supports
multiple architectures: ARM, x86, x86-64, PowerPC, and MIPS. BAP disassembles and lifts binary code into
the RISC-like BAP Instruction Language ([BIL](https://github.com/BinaryAnalysisPlatform/bil/releases/download/v0.1/bil.pdf)).
Program analysis is performed using the BIL representation and is architecture independent in a sense that it will work equally
well for all supported architectures. The platform comes with a set of tools, libraries, and plugins. The [documentation](http://binaryanalysisplatform.github.io/bap/api/master/argot_index.html) and [tutorial](https://github.com/BinaryAnalysisPlatform/bap-tutorial) are also available.
The main purpose of BAP is to provide a toolkit for implementing automated program analysis. BAP is written
in [OCaml](https://ocaml.org/) and it is the preferred language to write analysis, we have bindings to
[C](https://github.com/BinaryAnalysisPlatform/bap-bindings),
[Python](https://github.com/BinaryAnalysisPlatform/bap-python) and
[Rust](https://github.com/maurer/bap-rust). The Primus Framework also provide a Lisp-like DSL for writing program analysis tools.

BAP is developed in [CMU, Cylab](https://www.cylab.cmu.edu/) and is sponsored by various grants
from the United States Department of Defense, Siemens AG, and the Korea government, see [sponsors](#Sponsors) for more information.

# Table of contents
* [Installation](#installation)
  * [Binary](#binary)
  * [Sources](#from-sources)
* [Usage](#usage)
  * [Shell](#shell)
  * [OCaml](#ocaml)
  * [Python](#python)
  * [C](#c)
  * [baptop](#baptop)
* [Contributing](#contributing)
* [Sponsors](#sponsors)

# Installation

## Binary

We provide binary packages packed for Debian and Red Hat
derivatives. For other distributions we provide tgz archives. To
install bap on a Debian derivative:

```bash
wget https://github.com/BinaryAnalysisPlatform/bap/releases/download/v1.6.0/{bap,libbap,libbap-dev}_1.6.0.deb
sudo dpkg -i {bap,libbap,libbap-dev}_1.6.0.deb
```

## From sources

The binary release doesn't contain OCaml runtime, and is suitable only
if you are not going to extend BAP using OCaml programming language.
If you want to write your own analysis in OCaml, we recommend to use the OPAM package manager to
install BAP and the development environment.  After you've successfully
[installed](https://opam.ocaml.org/doc/Install.html) OPAM, run the
following commands:

```bash
opam init --comp=4.05.0              # install the compiler
eval `opam config env`               # activate opam environment
opam depext --install bap            # install bap
```

Got any problems? Then visit our [troubleshooting page](https://github.com/BinaryAnalysisPlatform/bap/wiki/Troubleshooting-installation)
or contact us directly via our Gitter [chat](https://gitter.im/BinaryAnalysisPlatform/bap).

# Usage

## Shell

The BAP main frontend is a command line utility called `bap`. You can
use it to explore the binary, run existing analysis, plugin your own
behavior, load traces, and much more.

To dump a program in various formats use the `--dump` option (or its short
equivalent, `-d`), For example, let's run `bap` on the
[x86_64-linux-gnu-echo](https://github.com/BinaryAnalysisPlatform/bap-testsuite/blob/master/bin/x86_64-linux-gnu-echo)
file.

```fortran
$ bap testsuite/bin/x86_64-linux-gnu-echo -d | grep 'sub print_endline' -A44
00000334: sub print_endline()
00000301:
00000302: v483 := RBP
00000303: RSP := RSP - 8
00000304: mem := mem with [RSP, el]:u64 <- v483
00000305: RBP := RSP
00000307: RSP := RSP - 0x20
0000030e: mem := mem with [RBP + 0xFFFFFFFFFFFFFFE8, el]:u64 <- RDI
0000030f: RAX := mem[RBP + 0xFFFFFFFFFFFFFFE8, el]:u64
00000310: mem := mem with [RBP + 0xFFFFFFFFFFFFFFF8, el]:u64 <- RAX
00000311: goto %00000312

00000312:
00000313: RAX := mem[RBP + 0xFFFFFFFFFFFFFFF8, el]:u64
00000314: RAX := pad:64[pad:32[mem[RAX]]]
00000315: v545 := low:8[low:32[RAX]]
0000031b: ZF := 0 = v545
0000031c: when ~ZF goto %0000032a
0000031d: goto %0000031e

0000031e:
0000031f: RDI := pad:64[0xA]
00000320: RSP := RSP - 8
00000321: mem := mem with [RSP, el]:u64 <- 0x400731
00000322: call @putchar with return %00000323

00000323:
00000324: RSP := RBP
00000325: RBP := mem[RSP, el]:u64
00000326: RSP := RSP + 8
00000327: v693 := mem[RSP, el]:u64
00000328: RSP := RSP + 8
00000329: return v693

0000032a:
0000032b: RAX := mem[RBP + 0xFFFFFFFFFFFFFFF8, el]:u64
0000032c: RDX := RAX + 1
0000032d: mem := mem with [RBP + 0xFFFFFFFFFFFFFFF8, el]:u64 <- RDX
0000032e: RAX := pad:64[pad:32[mem[RAX]]]
0000032f: RAX := pad:64[extend:32[low:8[low:32[RAX]]]]
00000330: RDI := pad:64[low:32[RAX]]
00000331: RSP := RSP - 8
00000332: mem := mem with [RSP, el]:u64 <- 0x40071C
00000333: call @putchar with return %00000312
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
plugin. The `--list-plugins` option also accepts a list of tags, that
will limit the output to plugins that match with the selected tags. For
the list of tags use the `--list-tags` option. You can also use
the `opam search bap` command, to get the information about bap packages,
available from OPAM.

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

## C

The same program in C will take too much space, and will not fit into
the README format, but this is an example, of a simple diassembler in C:

```c
#include <stdio.h>
#include <bap.h>

char data[] = "\x48\x8d\x00";

int main(int argc, const char **argv) {
    bap_init(argc, argv);

    if (bap_load_plugins() < 0) {
        fprintf(stderr, "Failed to load BAP plugins\n");
        return 1;
    }

    bap_disasm_basic_t *dis = bap_disasm_basic_create(BAP_ARCH_X86_64);

    if (!dis) {
        fprintf(stderr, "can't create a disassembler: %s\n", bap_error_get());
    }

    const int len = sizeof(data) - 1;
    bap_code_t *code = bap_disasm_basic_next(dis, data, sizeof(data) - 1, 0x80000);
    if (!code) {
        fprintf(stderr, "can't disassemble instruction: %s\n", bap_error_get());
        return 1;
    }
    bap_code_print(code);
    bap_release(code);
    bap_disasm_basic_close(dis);
    return 0;
}
```

The example can be compiled with the following command (assuming that
the code is in the `example.c` file):

```
make LDLIBS=-lbap example
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

# Learning

Other than
[API](https://binaryanalysisplatform.github.io/bap/api/master/argot_index.html)
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

# <a name="Sponsors"></a>Sponsors

* [DARPA VET Project](https://www.darpa.mil/program/vetting-commodity-it-software-and-firmware)
* [Siemens AG](https://www.siemens.com/us/en/home.html)
* Institute for Information & communications Technology Promotion(IITP) grant funded by the Korea government(MSIT)
  (No.2015-0-00565,Development of Vulnerability Discovery Technologies for IoT Software Security)

Please, [contact us](https://www.cylab.cmu.edu/partners/index.html) if you would like to become a sponsor.
