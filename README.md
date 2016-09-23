# Overview

[![Join the chat at https://gitter.im/BinaryAnalysisPlatform/bap](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/BinaryAnalysisPlatform/bap?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![docs](https://img.shields.io/badge/doc-1.0.0-green.svg)](http://binaryanalysisplatform.github.io/bap/api/v1.0.0/argot_index.html)
[![docs](https://img.shields.io/badge/doc-master-green.svg)](http://binaryanalysisplatform.github.io/bap/api/master/argot_index.html)
[![Build Status](https://travis-ci.org/BinaryAnalysisPlatform/bap.svg?branch=master)](https://travis-ci.org/BinaryAnalysisPlatform/bap)
[![pip](https://img.shields.io/badge/pip-1.1.0-green.svg)](https://pypi.python.org/pypi/bap/)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/BinaryAnalysisPlatform/bap/blob/master/LICENSE)

Binary Analysis Platform is a framework for writing program analysis
tools, that target binary files. The framework consists of a bunch
of libraries, plugins and frontends. The libraries provide code
reusability, plugins facilitate extensibility and frontends serve as
entry points.

# <a name="Installation"></a>Installation

We use OPAM package manager to handle installation. After you've successfully
[installed](https://opam.ocaml.org/doc/Install.html) opam, do the following:

```bash
$ opam init --comp=4.02.3    # install the compiler
$ opam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository
$ eval `opam config env`               # activate opam environment
$ opam depext --install bap            # install bap
```


# Usage

## OCaml

An idiomatic way of using BAP is to extend it with a plugin. Suppose,
you want to write some analysis. For example, let's estimate the ratio
of jump instructions to the total amount of instructions (a value that
probably correlates with a complexity of a program).

So, let's do it. Open your
[favorite text editor](https://github.com/BinaryAnalysisPlatform/bap/wiki/Emacs)
and write the following program in a file "jmp.ml" (make sure, that
you created a separate empty directory for your project):

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
visitor, that has a state consisting of a pair of counters. The first
counter keeps track of the amount of jmp terms, and the second counter
is incremented every time we enter any term.  The `main` function, just
runs the counter. We register it with `Project.register_pass'`, and our
function will be called by the system, as soon as project is loaded and
ready for the analysis.

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
$ bap /bin/true --pass=jmps
ratio = 974/7514 = 0.129625
$ bap /bin/ls --pass=jmps
ratio = 8917/64557 = 0.138126
```

## Python

OK, if the previous example doesn't make any sense for you, then you
can try our
[Python bindings](https://github.com/BinaryAnalysisPlatform/bap-python).
Install them with `pip install bap` (you still need to install `bap`
before hand). Here is the same example, but in Python:

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
program, that will interactively evaluate OCaml instructions, and
print results. Just run:

```bash
$ baptop
```

Now, you can play with BAP. The following example will open a file,
build callgraph of a program, and a control flow graph and dominance
tree of a function.

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
# #use "topfind";;
# #require "bap.top";;
```

## Shell

The BAP main frontend is a command line utility called
[bap](https://github.com/BinaryAnalysisPlatform/bap-python). You can
use it to explore the binary, run existing analysis, plugin your own
behavior, load traces, and many more.

To dump a program in various formats use `--dump` option (or its short
equivalent `-d`). By default it dumps in the IR, but you can choose
from various other formats. Use `--list-formats` option, to get the
list. Note, however, that formats are added by plugins, and there are
plugins lurking somewhere in opam, that may bring your favorite format
to bap, for example, `bap-piqi` package, brings support for a plethora
of formats (protobuf, xml, json, to name a few).

To discover what plugins are currently available, use `--list-plugins`
option. A short description will be printed for each plugin. You can
also use `opam search bap` command, to get the information about other
bap packages.

To get information about specific plugin named `<PLUGIN>` use
`--<PLUGIN>-help` option, e.g., `bap --llvm-help`.

The `bap` utility works with the whole binaries, if you have just few
bytes with which you would like to tackle, then `bap-mc` is what you
are looking for.

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
[API](http://binaryanalysisplatform.github.io/bap/api/v1.0.0/argot_index.html)
documentation, we have
[blog](http://binaryanalysisplatform.github.io/bap_plugins/) and
[wiki](https://github.com/BinaryAnalysisPlatform/bap/wiki/), where you
can find some useful information. Also, we have a permanently manned
chat in case of emergency. Look at the badge on top of the README
file, and feel free to join.

# Contributing

BAP is a framework, so you don't need to change its code to extend
it. We use dependency injection principle, with lots of injection
points, that allow you to alter BAP behavior. However, bugs happen, so
if you any problems, questions or suggestions, please, don't hesitate
to use our issue tracker. Submitting a pull request with a problem fix
will make us really happy.

If you wrote analysis with BAP, then don't hesitate to release it to
opam, for the benefit of the community.
