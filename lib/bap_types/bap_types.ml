(** Bap base types.

    This library introduces base types for Binary Analysis Platform.

    Library provides 7 core types, namely:

    - [arch] - computer architecture
    - [size] - word or addresses size
    - [var]  - a typed variable
    - [type] - types of expressions
    - [exp]  - bil expression
    - [stmt] - bil statement
    - [addr],
      [word] - represents imidiate data,
               this types are indeed synonyms to [Bitvector.t].

    Every type implements [Regular.S] interface. This interface is
    very similiar to core's [Identifiable], and is supposed to
    represent a type that is as common as built-in type. One should
    expect to find any function that is implemented for such types as
    [int], [string], [char], etc. To name a few, this interface
    includes:

    - comparison functions: ([<, >, <= , >= , compare, between, ...]);
    - each type defines a polymorphic [Map] with keys of type [t];
    - each type provides a [Set] with values of type [t];
    - for those, courage enough there is also an AVL tree;
    - hastable is exposed via [Table] module;
    - sexpable and binable interface;
    - [to_string], [str], [pp], [ppo], [pps] functions
      for pretty-printing.

    And most types usually provides much more.
*)

(**  {3 How to use the library }

     You should start any code relying on [bap-types] library with a

     [open Bap.Std]

     (or open [Bap_types.Std] if you're developing inside bap)

     It is a good idea, to open [Core_kenel.Std] before. You should
     never use modules that are not exposed by the [Std] module directly,
     if you need something from this, that export it via [Std] module,
     and make a pull request, or whatever. Although, while these
     modules are barred, their interfaces can still serve as good
     reference and documentation, that's why they are installed along
     with the library package.

     For each exported type, there is a module with the same name
     (module capitalization), that implements its interface. For
     example, type [exp] is indeed a type abbreviation for [Exp.t], and
     module [Exp] contains all functions and types related to type
     [exp]. Most modules consists of two parts:
     - base part, that usually defines types and constructors,
     - main part, that extends the type interface with common
     functions. This is hidden from a user, but it is good to know
     about this, since it will help you to find a documentation. For
     example, module [Stmt], that implements interface for type [stmt],
     consists of module [Bap_bil.Stmt] extended by [Bap_stmt] module,
     but from a user's perspective, you will see only [Stmt] module,
     with fields from both modules merged. For a reference purpose,
     although it is a good idea to look in [Bap_bil.Stmt] module for
     type definitions, and [Bap_stmt] for extra functions. But, again,
     you do not need to actually reference this modules from your code,
     for example, to create a hashtable of statements, just type:

     [let table = Stmt.Table.create ()]

     If a type is a variant type, and most types in [bap-types]
     library, are variant, then for each contructor named [Name], you
     will find a corresponding function named [name] that will accept
     the same number of arguments as the arity of the constructor. For
     example, type [exp] has a contructor [Extract int * int * t)], and
     there is a corresponding function named [extract], that has type
     [int -> int -> t -> t]. See [variantslib] for more information about,
     variants interface.

     {3 Linking, loading and other staff}

     Library is organized as a main library, named [bap-types], and
     several sublibraries, namely:

     - [top] for loading [bap-types] with all its dependencies to a
     ocaml's toplevel.


     - [bap-types.serialization] - provides facilities for marshaling
     and demarshaling base types;

     - [conceval] - for checking BIL semantics;

     {3 Using in toplevel }

     To start tackling with BIL and BAP in an OCaml's toplevel all that
     you need, is invoke the following commands in a toplevel
     (including '#' symbol):

     {[
       #use "topfind";;
       #require "bap-types.top";;
     ]}

     This will install pretty-printers for bap-types, types in core
     kernel, and perform all needed customizations.
*)

open Core_kernel.Std
open Bap_common

module Std = struct
  (** A definition for a regular type, and a handy module,
      that can create regular types out of thin air. *)
  module Regular = Regular
  module Integer = Integer
  module Printable = Printable

  module type Regular = Regular
  module type Integer = Integer
  module type Printable = Printable

  (** Processor architecture.

      Note: if you're looking for a [ARCH] signature it is in the very
      bottom of the file. *)
  module Arch = struct
    include Arch
    include Bap_arch
  end


  (** Typed and always fresh variables.  *)
  module Var  = Bap_var

  (** Types of BIL expressions  *)
  module Type = struct
    include Type
    include Bap_type
  end

  (** This module exports first-class type definitions,
      like [reg8_t], or [mem32_t]. Look at [Bap_type], for more. *)
  include Type.Export


  (** BIL expressions. *)
  module Exp = struct
    include Bap_bil.Exp
    include Bap_exp
    module Cast  = Cast
    module Binop = Binop
    module Unop  = Unop
  end

  (** Sizes of expression operands  *)
  module Size = struct
    include Size
    include Bap_size
  end

  (** Bil statements  *)
  module Stmt = struct
    include Bap_bil.Stmt
    include Bap_stmt
  end

  (** Bitvector is an ubiquitous module, that represents bitstrings and
      arbitrary sized numbers at once. It is used to represent
      addresses, imidiate operands and memories. It is suggested, that
      depending on a context, one should use [Addr] module and
      corresponding type to represent addresses, and [Word] module to
      denote other values. Although they all will point to the same
      type, it will be a little more explicit, and more understandable.
  *)
  module Bitvector = Bap_bitvector

  (** Address representation.
      This module not only includes [Bit_vector] implementation, but also
      adds some helpful functions, that makes good sense for addresses.
      Look at [Bap_addr] for more information.
  *)
  module Addr = struct
    include Bitvector
    include Bap_addr
  end

  (** A fancy abbreviation for bitvectors, that are supposed to
       represent data *)
  module Word = Bitvector

  (** Byte endian. This is the only not first class type in a bap-types.
      Sorry, no mapping and tables for this type.
  *)
  type endian = Bap_common.endian =
      LittleEndian | BigEndian
  with sexp,bin_io,compare

  (** {2 Type abbreviations}
      In this section there is a type abbreviation for all
      main types and auxiliary types.
  *)

  (** polymorphic sizes allows one to specify explicitly a set of
      acceptable sizes, e.g., [type mmx = [`r128 | `r256] poly_size]
  *)
  type 'a size_p = 'a Size.p
  with bin_io, compare, sexp

  (** [addr_size] is a subset of sizes that contains only two
      instances [`r32] and [`r64]  *)
  type nonrec addr_size = addr_size
  with bin_io, compare, sexp

  type size      = Size.t      with bin_io, compare, sexp
  type typ       = Type.t      with bin_io, compare, sexp
  type var       = Var.t       with bin_io, compare, sexp
  type stmt      = Stmt.t      with bin_io, compare, sexp
  type exp       = Exp.t       with bin_io, compare, sexp
  type bil       = stmt list   with bin_io, compare, sexp
  type arch      = Arch.t      with bin_io, compare, sexp
  type addr      = Addr.t      with bin_io, compare, sexp
  type word      = Word.t      with bin_io, compare, sexp
  type cast      = Exp.Cast.t  with bin_io, compare, sexp
  type unop      = Exp.Unop.t  with bin_io, compare, sexp
  type binop     = Exp.Binop.t with bin_io, compare, sexp


  module Seq = struct
    include Sequence
    include Bap_seq
  end

  include Seq.Export

  (** {2 Common type abbreviations}  *)
  type 'a seq = 'a Seq.seq with sexp
  type bigstring = Bigstring.t

  include Bap_int_conversions

end
