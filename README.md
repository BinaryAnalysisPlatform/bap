[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/BinaryAnalysisPlatform/bap/blob/master/LICENSE)
[![Join the chat at https://gitter.im/BinaryAnalysisPlatform/bap](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/BinaryAnalysisPlatform/bap?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![docs](https://img.shields.io/badge/doc-master-green.svg)][api-master]
[![docs](https://img.shields.io/badge/doc-2.0.0-green.svg)][api-2.0]
[![docs](https://img.shields.io/badge/doc-1.6.0-green.svg)][api-1.6]
[![Build Status](https://travis-ci.org/BinaryAnalysisPlatform/bap.svg?branch=master)][travis]

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
The `opam depext --install bap` command will try to fuflill the system dependencies of BAP, e.g., LLVM and is the common point of failure, especially on uncommon distributions or for rare versions of LLVM. If it fails, try to install the system depencies manually, using your operating system package manager, and then use the common `opam install bap` command, to install BAP. If it still doesn't work, do no hesitate to drop by our [chat][gitter] and seek help their. It is manned with friendly people that will be happy to help.

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

BAP, like Docker or Git, is driven by a single command line utility called `bap`. Just type `bap` in your shell and it will print a message which shows BAP capabilities. The `disassemble` command will take a binary program, disassemble it, lift it into the intermediate architecture agnostic representation, build a control flow graph, and finally apply staged user defined analysis in a form of disassembling passes. Finally, the `--dump` option (`-d` in short) will output the resulting program in the specified format. This is the default command, so you don't even need to specify it, e.g., the following will disassembled and dump the `/bin/echo` binary on your machine:
```bash
bap /bin/echo -d
```

Note, that unlike `objdump` this command will build the control flow graph of a program. If you just want to dump each instruction of a binary one after another (the so called linear sweep disassembler mode), then you can use the `objdump` command, e.g.,

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

# Extending

## Writing your own analysis

BAP is a plugin based framework and if you want to develop a new analysis you can write a plugin, build it, install, and it will work with the rest of the BAP without any recompilation. There are many extension points which you could use to add new analysis, change existing, or even build your own applications. We will start with a simple example, that registers a disassembling pass to the disassemble command. Suppose that we want to write an analysis that estimates the ratio of jump instructions to the total number of instruction in the binary. We will start with creating an empty file named `jmp.ml` in an empty folder (the folder name doesn't matter). Next, using our favorite text [editor][emacs] we will put the following code into it:

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
   Project.register_pass' main
```
Now we can build, install, and run our analysis using the following commands:
```
bapbuild jmp.plugin
bapbundle install jmp.plugin
bap /bin/echo --pass=jmp
```

Let's briefly go through the code. The `counter` object is a visitor that has the state consisting of a pair of counters. The first counter keeps track of the number of jmp terms, and the second counter is incremented every time we enter any term.  The `main` function just runs the counter and prints the output. We declare our extension use the [Extension.declare][extension-declare] function from the [Bap_main][bap-main] library. An extension is a just a function that receieves the context (which could be used to obtain configuration parameters). In this function we register our `main` function as a pass using the `Project.register_pass` function. 


## baptop

BAP also ships an interactive toplevel, aka REPL. This is a shell-like program that will interactively evaluate OCaml instructions and print the results. It will load BAP libraries and initalize all plugins for you, so you can iteractively explore the vast word of BAP. 

```bash
$ baptop
```

The `baptop` utility can also serve as non-interactive interpreter, so that you can run you ocaml scripts, e.g., `baptop myscript.ml` or you can even specify it using sha-bang at the top of your file, e.g., `#!/usr/bin/env baptop`. We built `baptop` using `utop`, but you can easily use any other OCaml toplevel, including `ocaml` itself, just load the `bap.top` library, e.g., for vanilla `ocaml` toplevel

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
[demo]: https://binaryanalysisplatform.github.io/assets/playfull.svg
[mayhem]: https://forallsecure.com/solutions/devsecops/
[cbat]: https://github.com/draperlaboratory/cbat_tools
[cwe-checker]: https://github.com/fkie-cad/cwe_checker
[cgc]: https://www.darpa.mil/program/cyber-grand-challenge
[troubleshoot]: https://github.com/BinaryAnalysisPlatform/bap/wiki/Troubleshooting-installation
[opam-install]: https://opam.ocaml.org/doc/Install.html
[contact-us]: https://www.cylab.cmu.edu/partners/index.html
[gitter]: https://gitter.im/BinaryAnalysisPlatform/bap
[travis]: https://travis-ci.org/BinaryAnalysisPlatform/bap
[wiki]: https://github.com/BinaryAnalysisPlatform/bap/wiki
[emacs]: https://github.com/BinaryAnalysisPlatform/bap/wiki/Emacs
[bap-main]: http://binaryanalysisplatform.github.io/bap/api/master/bap-main/Bap_main/index.html
[extension-declare]: http://binaryanalysisplatform.github.io/bap/api/master/bap-main/Bap_main/Extension/index.html#val-declare

[api-1.6]: http://binaryanalysisplatform.github.io/bap/api/v1.6.0/argot_index.html
[api-2.0]: http://binaryanalysisplatform.github.io/bap/api/odoc/index.html
[api-master]: http://binaryanalysisplatform.github.io/bap/api/master/index.html
