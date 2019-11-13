[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/BinaryAnalysisPlatform/bap/blob/master/LICENSE)
[![Join the chat at https://gitter.im/BinaryAnalysisPlatform/bap](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/BinaryAnalysisPlatform/bap?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![docs](https://img.shields.io/badge/doc-1.6.0-green.svg)](http://binaryanalysisplatform.github.io/bap/api/v1.6.0/argot_index.html)
[![docs](https://img.shields.io/badge/doc-master-green.svg)](http://binaryanalysisplatform.github.io/bap/api/master/index.html)
[![docs](https://img.shields.io/badge/doc-2.0.0-green.svg)](http://binaryanalysisplatform.github.io/bap/api/odoc/index.html)
[![Build Status](https://travis-ci.org/BinaryAnalysisPlatform/bap.svg?branch=master)](https://travis-ci.org/BinaryAnalysisPlatform/bap)

# Table of contents
* [Overview](#overview)
* [Installation](#installation)
* [Using](#usage)
* [Learning](#learning)
* [Contributing](#contributing)
* [Sponsors](#sponsors)

# Overview

The Carnegie Mellon University Binary Analysis Platform (CMU BAP) is a suite of utilities and libraries that enables analysis of programs in their machine representation. BAP includes an evergrowing set of ready to use [tools][toolkit] and provides various facilities for building custom tools, starting from various analysis-specific domain languages, such as, Primus Lisp, BML, BARE, Recipes, etc, which do not require sophisticated programming skills, and ending with implementing custom plugins in OCaml or even in Rust or C, via provided bindings.  The following short [demonstration][demo] of BAP capabilities is interactive, you can pause it at any moment and even copy the contents. 

BAP is developed in [CMU, Cylab](https://www.cylab.cmu.edu/) and is sponsored by grants from the United States Department of Defense, Siemens, Boening, ForallSecure, and the Korea government, see [sponsors](#Sponsors) for more information. BAP is used in various institutions and serves as a backbone for many interesting projects, some are highlighted below:
- [The CGC winner][cgc] [ForAllSecure Mayhem][mayhem];
- [Draper's Laboratory CBAT Tools][cbat];
- [Fraunhofer FKIE CWE Checker][cwe-checker].


# Installation

## Using pre-build packages

We provide binary packages packed for Debian and Red Hat derivatives. For other distributions we provide tgz archives. To install bap on a Debian derivative:

```bash
wget https://github.com/BinaryAnalysisPlatform/bap/releases/download/v2.0.0/{bap,libbap,libbap-dev}_2.0.0.deb
sudo dpkg -i {bap,libbap,libbap-dev}_2.0.0.deb
```

## From sources

Our binary packages do not include the OCaml development environment. If you are going to write an analysis in OCaml you need to install BAP from the source code using either [opam][opam-install] or by cloning and building this repository directly. The opam method is the recommended one. Once it is installed the following three commands should install the platform in a newly created switch.

```bash
opam init --comp=4.07.0              # install the compiler
eval `opam config env`               # activate opam environment
opam depext --install bap            # install bap
```
The `opam depext --install bap` command will try to fuflill the system dependencies of BAP, e.g., LLVM and is the common point of failure, especially on uncommon distributions or for rare versions of LLVM. If it fails, try to install the system depencies manually, using your operating system package manager, and then use the common `opam install bap` command, to install BAP. If it still doesn't work, do no hesitate to drop by our [chat][gitter] and seek help their. It is manned with friendly people which will be happy to help.

The instruction above will get you the latest stable release of BAP. If you're interested in our rolling releases, which are automatically updated every time a commit to the master branch happens, then you can add our testing repository to opam, with the following command
```bash
opam repo add  bap-testing git+https://github.com/BinaryAnalysisPlatform/opam-repository#testing
```

After it is added, the `bap-testing` repository will take precedence over the stable repository and you will get the freshly picked BAP packages straight from the farm.

If you insist on building BAP manually or just want to tackle with BAP internals, then you can clone this repository and build it manually. You will need to start with a fresh environment without BAP being installed, to prevent clashes. You will obviously need to install the dependencies of BAP, our `configure` script will help you figure out the missing libraries. Once all the dependencies are satisfied, the following tripplet will build and install the Platform:
```bash
./configure --enable-everything
make
make install 
```

The `configure` script lets you define a specific set of components that you need. We have nearly a hundred of components and naming them all will be too tedious, that's why we added the `--enable-everything` option. It plays nice with the `--disable-<feature>` component, so that you can unselect components that are not relevant to your current task. For more tips and tricks see our [wiki][wiki] and do not hestiate to tip back. We encourage everyone to use our wiki for collaboration and information sharing. And as always, drop by [gitter][gitter] for a friendly chat.

# Using

## Basic interactions

BAP, like docker or git, is driven by a single command line utility called `bap`. Just type `bap` in your shell and it will print a message which shows BAP capabilities. The `disassemble` command will take a binary program, disassemble it, lift it into the intermediate architecture agnostic representation, build a control flow graph, and finally apply staged user defined analysis in a form of disassembling passes. Finally, the `--dump` option will output the resulting program in the specified format. This is the default command, so you don't even need to specify it, e.g., the following will disassembled and dump the `/bin/echo` binary on your machine:
```bash
bap /bin/echo -d
```

Note, that unlike `objdump` this command will build the control flow graph of a program. If you just want to dump each instruction of a binary one after another (the so called linear sweep disassembler), then you can use the `objdump` command, e.g.,

```bash
bap objdump /bin/echo --show-{asm,bil}
```

If your input is a blob of machine code, not an executable, then you can use the `raw` loader, e.g.,
```bashthan a hundred components
bap objdump /bin/echo --loader=raw --loader-base=0x400000 
```

The raw loader takes a few parameters, like offsets, lenghts, and base addresses, which makes it a swiss-knife that you can use as a can opener for formats that are not known to BAP. The raw loader works for all commands that open files, e.g., if the `raw` loader is used together with the `disassemble` command, BAP will still automatically identify function starts and build a suitable CFG without even knowing where the code is in the binary,
```bash
bap /bin/echo --loader=raw --loader-base=0x400000 
```




The BAP main frontend is a command line utility called `bap`. You can
use it to explore the binary, run existing analysis, plugin your own
behavior, load traces, and much more.

To dump a program in various formats use the `--dump` option (or its short
equivalent, `-d`), For example, let's run `bap` on the
[x86_64-linux-gnu-echo](https://github.com/BinaryAnalysisPlatform/bap-testsuite/blob/master/bin/x86_64-linux-gnu-echo)
file.



## Writing a simple disassembling pass

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
 
 Please, [contact us][contact-us] if you would like to become a sponsor or are seeking a deeper collaboration. 
  
[toolkit]: https://github.com/BinaryAnalysisPlatform/bap-toolkit
[demo]: http://binaryanalysisplatform.github.io/assets/playfull.svg
[mayhem]: https://forallsecure.com/solutions/devsecops/
[cbat]: https://github.com/draperlaboratory/cbat_tools
[cwe-checker]: https://github.com/fkie-cad/cwe_checker
[cgc]: https://www.darpa.mil/program/cyber-grand-challenge
[troubleshoot]: https://github.com/BinaryAnalysisPlatform/bap/wiki/Troubleshooting-installation
[opam-install]: https://opam.ocaml.org/doc/Install.html
[contact-us]: https://www.cylab.cmu.edu/partners/index.html
[gitter]: https://gitter.im/BinaryAnalysisPlatform/bap
[wiki]: https://github.com/BinaryAnalysisPlatform/bap/wiki
