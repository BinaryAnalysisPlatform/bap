(** Bap base types.

    This library introduces base types for Binary Analysis Platform.

*)


open Core_kernel.Std
open Regular.Std
open Bap_common

(** This module is included into [Bap.Std], you need to open it
    specifically if you're developing inside BAP *)
module Std = struct
  (** A definition for a regular type, and a handy module,
      that can create regular types out of thin air. *)
  module Integer = Integer
  module State = Bap_state
  module Trie = struct
    include Bap_trie_intf
    include Bap_trie
  end

  module type Integer = Integer
  module type Trie = Trie

  module Monad = struct
    include Bap_monad
    include Bap_monad_types
  end


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

  module Bil = struct
    type t = Bap_bil.bil [@@deriving bin_io, compare, sexp]
    include (Bap_stmt.Stmts_pp : Printable with type t := t)
    include (Bap_stmt.Stmts_data : Data with type t := t)
    module Types = struct
      include Bap_bil.Cast
      include Bap_bil.Binop
      include Bap_bil.Unop
      include Bap_bil.Exp
      include Bap_bil.Stmt
    end
    include Types
    module Infix = struct
      include Bap_exp.Infix
      include Bap_stmt.Infix
    end
    include Infix
    include Bap_exp.Exp
    include Bap_exp.Unop
    include Bap_exp.Binop
    include Bap_exp.Cast
    include Bap_stmt.Stmt
    include Bap_visitor
    include Bap_helpers
    module Result = Bap_result
    module Storage = Result.Storage
    class type storage = Result.storage
    type value = Result.value =
      | Imm of word
      | Mem of storage
      | Bot

    type result = Result.t
  end

  module Expi = Bap_expi
  module Bili = Bap_bili
  module Biri = Bap_biri
  module Type_error = Bap_type_error
  module Context = Bap_context

  type type_error = Type_error.t [@@deriving bin_io, compare, sexp]

  class ['a] bili = ['a] Bili.t
  class ['a] expi = ['a] Expi.t
  class ['a] biri = ['a] Biri.t

  (** [Regular] interface for BIL expressions *)
  module Exp = struct
    type t = Bap_bil.exp [@@deriving bin_io, compare, sexp]
    include Bap_helpers.Exp
    include (Bap_exp : Regular with type t := t)
    let pp_adt = Bap_bil_adt.pp_exp
  end

  (** [Regular] interface for BIL statements  *)
  module Stmt = struct
    type t = Bap_bil.stmt [@@deriving bin_io, compare, sexp]
    include Bap_helpers.Stmt
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

  module Value = Bap_value
  module Dict = Value.Dict

  (** Byte endian. This is the only not first class type in a bap-types.
      Sorry, no maps and tables for this type.
  *)
  type endian = Bap_common.endian = LittleEndian | BigEndian
    [@@deriving sexp, bin_io, compare]

  (** {2 Type abbreviations}
      In this section there is a type abbreviation for all
      main types and auxiliary types.
  *)

  (** polymorphic sizes allows one to specify explicitly a set of
      acceptable sizes, e.g., [type mmx = [`r128 | `r256] poly_size]
  *)
  type 'a size_p = 'a Size.p
    [@@deriving bin_io, compare, sexp]

  (** [addr_size] is a subset of sizes that contains only two
      instances [`r32] and [`r64]  *)
  type nonrec addr_size = Bap_common.addr_size
    [@@deriving bin_io, compare, sexp]

  type addr  = Addr.t      [@@deriving bin_io, compare, sexp]
  type arch  = Arch.t      [@@deriving bin_io, compare, sexp]
  type bil   = Bap_bil.bil [@@deriving bin_io, compare, sexp]
  type binop = Bil.binop   [@@deriving bin_io, compare, sexp]
  type cast  = Bil.cast    [@@deriving bin_io, compare, sexp]
  type exp   = Exp.t       [@@deriving bin_io, compare, sexp]
  type size  = Size.t      [@@deriving bin_io, compare, sexp]
  type stmt  = Stmt.t      [@@deriving bin_io, compare, sexp]
  type typ   = Type.t      [@@deriving bin_io, compare, sexp]
  type unop  = Bil.unop    [@@deriving bin_io, compare, sexp]
  type var   = Var.t       [@@deriving bin_io, compare, sexp]
  type word  = Word.t      [@@deriving bin_io, compare, sexp]
  type nat1  = int         [@@deriving bin_io, compare, sexp]
  type value = Value.t     [@@deriving bin_io, compare, sexp]
  type dict  = Value.dict  [@@deriving bin_io, compare, sexp]
  type 'a tag = 'a Value.tag

  class ['a] exp_visitor = ['a] Bap_visitor.bil_visitor
  class ['a] bil_visitor = ['a] Bap_visitor.bil_visitor

  module Vector = Bap_vector

  type 'a vector = 'a Vector.t [@@deriving bin_io, compare, sexp]

  (** {2 Common type abbreviations}  *)
  type bigstring = Bigstring.t

  include Bap_int_conversions
  include Bap_attributes

  (** Library configuration, version, and other constants  *)
  module Config = Bap_config

  module Seq = Seq
  type 'a seq = 'a Seq.t [@@deriving bin_io, compare, sexp]

  module Callgraph = Bap_ir_callgraph



end
