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

     - [conceval] - for evaluating BIL expressions
       (but see also [Bil.fold_consts]);
*)

open Core_kernel.Std
open Bap_common

(** This module is included into [Bap.Std], you need to open it
    specifically if you're developing inside BAP *)
module Std = struct
  (** A definition for a regular type, and a handy module,
      that can create regular types out of thin air. *)
  module Regular = Regular
  module Integer = Integer
  module Printable = Printable
  module Trie = Trie

  module type Regular = Regular
  module type Integer = Integer
  module type Printable = Printable
  module type Trie = Trie

  (** Target architecture. *)
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

  (** Sizes of expression operands  *)
  module Size = struct
    include Size
    include Bap_size
  end

  (** BIL

      This module captures BIL language definitions. Do not open
      this module globally, since it may override common OCaml
      functions. Actually, opening this module is a way of
      switching from OCaml language into a BIL embedded DSL, for
      example:

      {[
        Bil.([
            v := exp_of_op src lsr int32 1;
            r := exp_of_op src;
            s := int32 31;
            while_ (var v <> int32 0) [
              r := var r lsl int32 1;
              r := var r lor (var v land int32 1);
              v := var v lsr int32 1;
              s := var s - int32 1;
            ];
            dest := var r lsl var s;
          ])
      ]}

      In the above example, all operators, e.g., [lsl], [-], [<>],
      are from the BIL language, and the result of the above
      expression is a value of type [Bil.t].

      To summarize, this module includes:
      - all constructors
      - all infix and prefix operators of BIL
      - a set of functional constructors
      - BIL visitors
      - BIL helper functions

      This module also implements [Sexpable], [Binable] and
      [Printable] interfaces for the [bil] type aka [stmt list].
      This interfaces allow you to print, store and read BIL in
      different formats. There is also a standalone [Bil_piqi]
      module that provides a family of functions to store and load
      BIL in different formats, supported by piqi library. See,
      [bap-mc] application for examples of serializing BIL into
      different formats.

      The [Regular] interface for the [bil] type is not provided for
      intention, to prevent from creating [Bil.Map], [Bil.Table] or
      other structures, that inefficient by their nature.

      If you're looking for the [Regular] interface for BIL
      expressions and statements, then [Exp] and [Stmt] modules below
      are for you. E.g., [Exp.(x = y)] will compare two expressions
      and will evaluate to the [bool] type. [Bil.(x = y)] results in
      a symbolic comparison of two expressions and will evaluate to a
      BIL expression.

  *)
  module Bil = struct
    type t = Bap_bil.bil with bin_io, compare, sexp
    include (Bap_stmt.Stmts_pp : Printable with type t := t)

    (** This submodule is useful if all you want is to get access
        to constructor names (for example for pattern matching).
        If opened this module will bring to the namespace only
        constructor names and types. Types will have the same name
        as corresponding module, e.g. [cast], [binop], [unop].
        It is quite safe to open this module globally. *)
    module Types = struct
      include Bap_bil.Cast
      include Bap_bil.Binop
      include Bap_bil.Unop
      include Bap_bil.Exp
      include Bap_bil.Stmt
    end
    (** put all types and constructor names into [Bil] namespace *)
    include Types

    (** {4 Infix operators}

        Every BIL expression or statement is mapped to the
        corresponding OCaml operator. Signed forms of operations
        a build with appending [$] sign to the original operator,
        e.g., signed division is [/%]. Exception, signed modulo
        is [%$].

        Move statement has an infix form of the assignment operator,
        i.e., [:=]. *)
    module Infix = struct
      include Bap_exp.Infix
      include Bap_stmt.Infix
    end
    include Infix

    (** {4 Functional constructors}

        For each constructor there is the same named function, c.f.,
        [variantslib] library, but with some exceptions summarized below.

        In general constructors are mapped to the same named
        functions, e.g.,   [Move of var * exp] is mapped to
        [move : var -> exp -> stmt].

        Some complex constructors (especially those with many
        parameters of the same type) are mapped functions with keyword
        arguments, e.g., [Load of (exp * exp * endian * size)] is
        mapped to [load mem:exp -> addr:exp -> endian -> size -> exp].

        Constructors that results in a keywords, e.g., [While], [If],
        are escaped with trailing underscore, e.g., [while_], [if_].
    *)
    include Bap_exp.Exp
    include Bap_exp.Unop
    include Bap_exp.Binop
    include Bap_exp.Cast
    include Bap_stmt.Stmt

    (** {4 Visitors}  *)
    include Bap_visitor

    (** {4 High level helpers}  *)
    include Bap_helpers
  end

  (** [Regular] interface for BIL expressions *)
  module Exp = struct
    type t = Bap_bil.exp with bin_io, compare, sexp
    include (Bap_exp : Regular with type t := t)
    let pp_adt = Bap_bil_adt.pp_exp
  end

  (** [Regular] interface for BIL statements  *)
  module Stmt = struct
    type t = Bap_bil.stmt with bin_io, compare, sexp
    include (Bap_stmt : Regular with type t := t)
    let pp_adt = Bap_bil_adt.pp_stmt
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
  type endian = Bap_common.endian = LittleEndian | BigEndian
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

  type addr      = Addr.t      with bin_io, compare, sexp
  type arch      = Arch.t      with bin_io, compare, sexp
  type bil       = Bap_bil.bil with bin_io, compare, sexp
  type binop     = Bil.binop   with bin_io, compare, sexp
  type cast      = Bil.cast    with bin_io, compare, sexp
  type exp       = Exp.t       with bin_io, compare, sexp
  type size      = Size.t      with bin_io, compare, sexp
  type stmt      = Stmt.t      with bin_io, compare, sexp
  type typ       = Type.t      with bin_io, compare, sexp
  type unop      = Bil.unop    with bin_io, compare, sexp
  type var       = Var.t       with bin_io, compare, sexp
  type word      = Word.t      with bin_io, compare, sexp

  class ['a] bil_visitor = ['a] Bap_visitor.visitor

  (** A more concise name for the Core's Sequence module.
      Also, it extends sequence with some useful functions.  *)
  module Seq = struct
    include Sequence
    include Bap_seq
  end
  include Seq.Export

  (** {2 Common type abbreviations}  *)
  type 'a seq = 'a Seq.seq with sexp
  type bigstring = Bigstring.t

  include Bap_int_conversions

  (** Library configuration, version, and other constants  *)
  module Config = Bap_config
end
