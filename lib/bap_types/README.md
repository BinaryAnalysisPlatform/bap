# bap-types Overview

The `bap-types` package contains the OCaml types used for the BAP
lifting and analysis system.  This package does nothing by itself; it
merely lays the foundation for other utilities such as `bap-lifter`.


The main files in this module are:
* `bil.ml`: This file contains the definition of BIL. Note that
  "Intermediate Language" is a misnomer; since v1.0 we do not intend
  for anyone to program in BIL. Although intermediate representation
  (IR) is more accurate, we use the name BIL for historical reasons.

* `bitvector.ml`: Computer values are modeled in BIL as of type
  `Bitvector.t`. All uses of values should be abstracted using this
  type.
  
* `type.ml`: This file contains the type of binary operations, unary
  operations, register types, cast types, and values supported by BIL.

* `arch.ml`: This file describes the architectures we support, and
  specifies the `ARCH` functor.

* `var.ml`: This file specifies how variables are created in our
  language, as well as utility functions that, among other things,
  allow for efficient variable equality tests.

* `pp.ml`: This file is a pretty printer for BIL.

* `conceval.ml`: This file is a concrete evaluator. It can be used as
  a specification for concrete semantics of BIL.
  

# Building
If you need to build from source, you will need to use the `opam`
OCaml package management system.  Please make sure you have it
installed.  We test building our tools on Mac, Windows, and Linux.

First, install the dependencies:
```
 $ opam install zarith piqi piqilib core_kernel
```

If this fails, please check the `META` file to make sure there isn't a
new prerequisite.

Then build, type `make`. To install, type `make install`

If you have trouble building, and are not familiar with OCaml or the
package management system, we suggest you start there.

XXX: Add where binary downloads are at.

# Development
Consistent code is beautiful code.  Please read our OCaml
development guide before editing these files. In particular, please
pay attention to our naming and coding conventions.

# License
Please see the LICENSE file for licensing information.

# TODO
Want to help out?  Here are some things to do:
* Create a specification for BIL outside `conceval.ml`
* The `Bitvector` module really is worthy of its own project a la
  Data.BitVector in Haskell.
  
