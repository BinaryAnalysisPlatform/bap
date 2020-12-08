# Binary Analysis Platform
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/BinaryAnalysisPlatform/bap/blob/master/LICENSE)
[![Join the chat at https://gitter.im/BinaryAnalysisPlatform/bap](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/BinaryAnalysisPlatform/bap?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![docs](https://img.shields.io/badge/doc-master-green.svg)][docs]
[![docs](https://img.shields.io/badge/doc-2.2.0-green.svg)][docs]
[![Build Status](https://travis-ci.org/BinaryAnalysisPlatform/bap.svg?branch=master)][travis]

## Table of contents
*   [Overview](#overview)
*   [Installation](#installation)
*   [Using](#using)
*   [Learning](#learning)
*   [Contributing](#contributing)
*   [Sponsors](#sponsors)

## Overview

The Carnegie Mellon University Binary Analysis Platform (CMU BAP) is a suite of utilities and libraries that enables analysis of programs in the machine code representation. BAP supports x86, x86-64, ARM, MIPS, PowerPC and new architectures can be added as plugins. BAP includes various analyses, standard interpreter, microexecution interpreter, and symbolic executor. BAP features its own domain-specific language, [Primus Lisp][primus-lisp], that is used for implementing analyses, specifying verification conditions, modeling functions (writing stubs) and even interfacing with the SMT solver. The [toolkit][toolkit] repository includes various examples of program analysis tools that could be implemented with BAP and can be used as the starting point (in addition to the [tutorial][bap-tutorial]) for implementing custom analyses. BAP can be used as a framework with a single [bap][demo] utility that is [extended with plugins][extending] or it can be used as a library [embedded][embedding] in a user application, which could be written in OCaml or, in any other language, using [C bindings][bap-bindings]. We also provide some [minimal support for Python][bap-python] to make it easier to start learning BAP.

BAP is developed in [CMU, Cylab](https://www.cylab.cmu.edu/) and is sponsored by grants from the United States Department of Defense, Siemens, Boeing, ForAllSecure, and the Korea government, see [sponsors](#Sponsors) for more information. BAP is used in various institutions and serves as a backbone for many interesting projects, some are highlighted below:
*   [The CGC winner][cgc] [ForAllSecure Mayhem][mayhem]
*   [Draper's Laboratory CBAT Tools][cbat]
*   [Fraunhofer FKIE CWE Checker][cwe-checker]

## Installation

### Using pre-build packages

We provide binary packages packed for Debian and Red Hat derivatives. For other distributions we provide tgz archives. To install bap on a Debian derivative:

```bash
wget https://github.com/BinaryAnalysisPlatform/bap/releases/download/v2.2.0/{bap,libbap,libbap-dev}_2.2.0.deb
sudo dpkg -i {bap,libbap,libbap-dev}_2.2.0.deb
```

### From sources

Our binary packages do not include the OCaml development environment. If you are going to write an analysis in OCaml you need to install BAP from the source code using either [opam][opam-install] or by cloning and building this repository directly. The opam method is the recommended one. Once it is installed the following three commands should install the platform in a newly created switch.

```bash
opam init --comp=4.09.0              # inits opam and install the OCaml compiler
eval `opam config env`               # activates opam environment
opam depext --install bap            # installs bap and its dependencies
```
The `opam depext --install bap` command will try to fulfill the system dependencies of BAP, e.g., LLVM and is the common point of failure, especially on uncommon distributions or for rare versions of LLVM. If it fails, try to install the system dependencies manually, using your operating system package manager, and then use the common `opam install bap` command, to install BAP. If it still doesn't work, do not hesitate to drop by our [chat][gitter] and seek help there. It is manned with friendly people that will be happy to help.

The instruction above will get you the latest stable release of BAP. If you're interested in our rolling releases, which are automatically updated every time a commit to the master branch happens, then you can add our testing repository to opam, with the following command
```bash
opam repo add  bap-testing git+https://github.com/BinaryAnalysisPlatform/opam-repository#testing
```

After it is added, the `bap-testing` repository will take precedence over the stable repository and you will get the freshly picked BAP packages straight from the farm.

If you insist on building BAP manually or just want to tackle with BAP internals, then you can clone this repository and build it manually. You will need to start with a fresh environment without BAP being installed, to prevent clashes. You will obviously need to install the dependencies of BAP, our `configure` script will help you figure out the missing libraries. Once all the dependencies are satisfied, the following triplet will build and install the Platform:
```bash
./configure --enable-everything
make
make install
```

The `configure` script lets you define a specific set of components that you need. We have nearly a hundred of components and naming them all will be too tedious, that's why we added the `--enable-everything` option. It plays nice with the `--disable-<feature>` component so that you can unselect components that are not relevant to your current task. For more tips and tricks see our [wiki][wiki] and do not hesitate to tip back. We encourage everyone to use our wiki for collaboration and information sharing. And as always, drop by [gitter][gitter] for a friendly chat.

## Using

BAP, like Docker or Git, is driven by a single command-line utility called [bap][man-bap]. Just type `bap` in your shell and it will print a message which shows BAP capabilities. The `disassemble` command will take a binary program, disassemble it, lift it into the intermediate architecture agnostic representation, build a control flow graph, and finally apply staged user-defined analysis in a form of disassembling passes. Finally, the `--dump` option (`-d` in short) will output the resulting program in the specified format. This is the default command, so you don't even need to specify it, e.g., the following will disassembled and dump the `/bin/echo` binary on your machine:
```bash
bap /bin/echo -d
```

Note, that unlike `objdump` this command will build the control flow graph of a program. If you just want to dump each instruction of a binary one after another (the so-called linear sweep disassembler mode), then you can use the `objdump` command, e.g.,

```bash
bap objdump /bin/echo --show-{insn=asm,bil}
```

If your input is a blob of machine code, not an executable, then you can use the `raw` loader, e.g.,

```bash
bap objdump /bin/echo --loader=raw --raw-base=0x400000 --show-{insn=asm,bil}
```

The raw loader takes a few parameters, like offsets, lengths, and base addresses, which makes it a swiss-knife that you can use as a can opener for formats that are not known to BAP. The raw loader works for all commands that open files, e.g., if the `raw` loader is used together with the `disassemble` command, BAP will still automatically identify function starts and build a suitable CFG without even knowing where the code is in the binary,

```bash
bap /bin/echo --loader=raw --raw-base=0x400000 -d
```

If you would like to play manually with bytes, e.g., type the instruction encoding manually and see how BAP disassembles it and what semantics it has, then `mc` is the command you're looking for. It is named for the corresponding utility in LLVM and stands for machine code and has the same interface as the `objdump` command except that it takes an ASCII encoding of instruction instead of a binary file, e.g.,

```bash
bap mc --show-{insn=asm,bil} -- 48 83 ec 08
```
or
```
bap mc --show-{insn=asm,bil} "\x48\x83\xec\x08"
```

It recognizes a few input formats (including `llvm-mc` is using for its `-show-encoding` option). Consult the documentation for more detailed information.

## Extending

### Writing your own analysis

BAP is a plugin-based framework and if you want to develop a new analysis you can write a plugin, build it, install, and it will work with the rest of the BAP without any recompilation. There are many extension points that you could use to add new analysis, change existing, or even build your own applications. We will start with a simple example, that registers a disassembling pass to the disassemble command. Suppose that we want to write an analysis that estimates the ratio of jump instructions to the total number of instructions in the binary. We will start by creating an empty file named `jmp.ml` in an empty folder (the folder name doesn't matter). Next, using our favorite text [editor][emacs] we will put the following code into it:

```ocaml
open Core_kernel
open Bap_main
open Bap.Std

let counter = object
  inherit [int * int] Term.visitor
  method! enter_term _ _ (jmps,total) = jmps,total+1
  method! enter_jmp _ (jmps,total) = jmps+1,total
end

let main proj =
  let jmps,total = counter#run (Project.program proj) (0,0) in
  printf "ratio = %d/%d = %g\n" jmps total (float jmps /. float total)

let () = Extension.declare @@ fun _ctxt ->
   Project.register_pass' main;
   Ok ()
```
Now we can build, install, and run our analysis using the following commands:
```
bapbuild jmp.plugin
bapbundle install jmp.plugin
bap /bin/echo --pass=jmp
```

Let's briefly go through the code. The `counter` object is a visitor that has the state consisting of a pair of counters. The first counter keeps track of the number of jmp terms, and the second counter is incremented every time we enter any term.  The `main` function just runs the counter and prints the output. We declare our extension use the [Extension.declare][extension-declare] function from the [Bap_main][bap-main] library. An extension is just a function that receives the context (which could be used to obtain configuration parameters). In this function, we register our `main` function as a pass using the `Project.register_pass` function.

A little bit more complex example, as well as an example that uses Python, can be found in our [tutorial][bap-tutorial].

### Interactive REPL

BAP also ships an interactive toplevel utility `baptop`. This is a shell-like utility that interactively evaluates OCaml expressions and prints their values. It will load BAP libraries and initialize all plugins for you, so you can interactively explore the vast world of BAP. The `baptop` utility can also serve as a non-interactive interpreter, so that you can run your OCaml scripts, e.g., `baptop myscript.ml` or you can even specify it using sha-bang at the top of your file, e.g., `#!/usr/bin/env baptop`. We built `baptop` using UTop, but you can easily use any other OCaml toplevel, including `ocaml` itself, just load the `bap.top` library, e.g., for vanilla `ocaml` toplevel use the following directives

```ocaml
#use "topfind";;
#require "bap.top";;
```

## Learning

We understand that BAP is huge and it is easy to get lost. We're working constantly on improving documentation ensuring that every single function in [BAP API][docs] is thoroughly documented. But writing higher-level guidelines in the form of manuals or [tutorials][bap-tutorial] is much harder and very time consuming, especially given how different the goals of our fellow researchers and users. Therefore we employ a backward-chaining approach and prefer to answer real questions rather than prematurely trying to address all possible questions. We will be happy to see you in your [chat][gitter] that features searchable, indexed by Google, archive.

We are writing, occasionally, to our [blog][blog] and [wiki][wiki] and are encouraging everyone to contribute to both of them. You can also post your questions on [stackoverflow][so-ocaml] or discuss BAP on the [OCaml][discuss-bap] board. We also have a cute [discord][discord-bap] channel, which has much less traffic than our [gitter][gitter].

## Contributing

BAP is built by the community and we're welcome all contributions from authors that are willing to share them under the MIT license. If you don't think that your analysis or tool suits this repository (e.g., it has a limited use, not fully ready, doesn't meet our standards, etc), then you can consider contributing to our [bap-plugins][bap-plugins] repository that is a collection of useful BAP plugins that are not mature enough to be included in the main distribution. Alternatively, you can consider extending our [toolkit][toolkit] with your tool.

Of course, there is no need to submit your work to one of our repositories. BAP is a plugin-based framework and your code could be hosted anywhere and have any license (including proprietary). If you want to make your work available to the community it would be a good idea to release it via [opam][opam-packaging].

## Sponsors
*   [ForAllSecure][fas]

*   [Boeing][boeing]

*   [DARPA VET Project](https://www.darpa.mil/program/vetting-commodity-it-software-and-firmware)

*   [Siemens AG](https://www.siemens.com/us/en/home.html)

*   Institute for Information & communications Technology Promotion(IITP) grant funded by the Korea government(MSIT) (No.2015-0-00565, Development of Vulnerability Discovery Technologies for IoT Software Security)

Please, [contact us][contact-us] if you would like to become a sponsor or are seeking a deeper collaboration.

[toolkit]: https://github.com/BinaryAnalysisPlatform/bap-toolkit
[bap-plugins]: https://github.com/BinaryAnalysisPlatform/bap-plugins
[bap-bindings]: https://github.com/BinaryAnalysisPlatform/bap-bindings
[bap-tutorial]: https://github.com/BinaryAnalysisPlatform/bap-tutorial
[bap-python]: https://github.com/BinaryAnalysisPlatform/bap-python
[primus-lisp]: https://binaryanalysisplatform.github.io/bap/api/master/bap-primus/Bap_primus/Std/Primus/Lisp/index.html
[extending]: https://binaryanalysisplatform.github.io/bap/api/master/bap-main/Bap_main/index.html#extending-bap
[embedding]: https://binaryanalysisplatform.github.io/bap/api/master/bap-main/Bap_main/index.html#embedding-bap
[demo]: https://binaryanalysisplatform.github.io/assets/playfull.svg
[mayhem]: https://forallsecure.com/solutions/devsecops/
[fas]: https://forallsecure.com/
[boeing]: https://www.boeing.com
[cbat]: https://github.com/draperlaboratory/cbat_tools
[cwe-checker]: https://github.com/fkie-cad/cwe_checker
[cgc]: https://www.darpa.mil/program/cyber-grand-challenge
[opam-install]: https://opam.ocaml.org/doc/Install.html
[contact-us]: https://www.cylab.cmu.edu/partners/index.html
[gitter]: https://gitter.im/BinaryAnalysisPlatform/bap
[travis]: https://travis-ci.org/BinaryAnalysisPlatform/bap
[wiki]: https://github.com/BinaryAnalysisPlatform/bap/wiki
[blog]: https://binaryanalysisplatform.github.io/
[so-ocaml]: https://stackoverflow.com/questions/tagged/ocaml
[discuss-bap]: https://discuss.ocaml.org/tags/bap
[discord-bap]: https://discord.gg/bwJ3p7q
[emacs]: https://github.com/BinaryAnalysisPlatform/bap/wiki/Emacs
[opam-packaging]: https://opam.ocaml.org/doc/Packaging.html
[bap-main]: http://binaryanalysisplatform.github.io/bap/api/master/bap-main/Bap_main/index.html
[extension-declare]: http://binaryanalysisplatform.github.io/bap/api/master/bap-main/Bap_main/Extension/index.html#val-declare

[docs]: https://binaryanalysisplatform.github.io/bap/api/master/index.html
[man-bap]: http://binaryanalysisplatform.github.io/bap/api/man/bap.1.html
