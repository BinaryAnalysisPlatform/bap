(*
 * Copyright (c) 2010-2014,
 *  Jinseong Jeon <jsjeon@cs.umd.edu>
 *  Kris Micinski <micinski@cs.umd.edu>
 *  Jeff Foster   <jfoster@cs.umd.edu>
 * All rights reserved.
 *
 * Based on the src/dex.mli from https://github.com/plum-umd/redexer
 *)

(* Format reference is https://source.android.com/devices/tech/dalvik/dex-format.html
 *)

(***********************************************************************)
(* Dex                                                                 *)
(***********************************************************************)

(** This module defines types for DEX binary and provides utility functions
 for traversing DEX file and getting info from DEX file. *)

(** {2 Types} *)

(** raise if something is logically incorrect *)
exception Wrong_dex of string

(** raise if there is no other cases for match block *)
exception Wrong_match of string

(** raise when attempting to get last instruction of method that has
    no return *)
exception No_return

(** raise if something is not implemented yet *)
exception NOT_YET of string

(** The top-level representation of a DEX binary file *)
type dex = {
  header          : dex_header;
  d_string_ids    : link DynArray.t;
  d_type_ids      : link DynArray.t;
  d_proto_ids     : proto_id_item  DynArray.t;
  d_field_ids     : field_id_item  DynArray.t;
  d_method_ids    : method_id_item DynArray.t;
  d_class_defs    : class_def_item DynArray.t;
  mutable d_data  : data_item Instr.IM.t;
}

(** encapsulation of in/direct access *)
and link =
  | Idx of int
  | Off of Instr.offset

(** header_item format *)
and dex_header = {
  magic           : string;
  checksum        : int64;
  signature       : char list;
  mutable file_size : int;
  header_size     : int;
  endian_tag      : endian;
  link            : section;
  map_off         : link;
  h_string_ids    : section;
  h_type_ids      : section;
  h_proto_ids     : section;
  h_field_ids     : section;
  h_method_ids    : section;
  h_class_defs    : section;
  h_data          : section;
}

(** [endian_tag] within {!dex_header} *)
and endian  =
  | LITTLE        (** ENDIAN_CONSTANT         = 0x12345678 *)
  | BIG           (** REVERSE_ENDIAN_CONSTANT = 0x78563412 *)

(** a pair of size and offset, used at {!dex_header} *)
and section = {
  size            : int;
  offset          : link;
}

(** {!proto_id_item} appears in the [d_proto_ids] *)
and proto_id_item = {
  shorty          : link;
  mutable return_type : link;
  parameter_off   : link;
}

(** {!field_id_item} appears in the [d_field_ids] *)
and field_id_item = {
  f_class_id      : link;
  mutable f_type_id : link;
  f_name_id       : link;
}

(** {!method_id_item} appears in the [d_method_ids] *)
and method_id_item = {
  m_class_id      : link;
  m_proto_id      : link;
  m_name_id       : link;
}

(** {!class_def_item} appears in the [d_class_defs] *)
and class_def_item = {
  c_class_id      : link;
  mutable c_access_flag : int;
  mutable superclass : link;
  mutable interfaces : link;
  source_file     : link;
  annotations     : link;
  mutable class_data : link;
  static_values   : link;
}

(** items in the data pool, which appears in the [d_data] *)
and data_item =
  | MAP_LIST      of map_item list
  | TYPE_LIST     of link list
  | ANNO_SET_REF  of link list (** [annotation_set_ref_list] *)
  | ANNO_SET      of link list (** [annotation_set_item] *)
  | CLASS_DATA    of class_data_item
  | CODE_ITEM     of code_item
  | STRING_DATA   of UTF8.t    (** same as [string] *)
  | DEBUG_INFO    of debug_info_item
  | ANNOTATION    of annotation_item
  | STATIC_VALUE  of encoded_value list (** [encoded_array] *)
  | ANNO_DIR      of anno_dir_item
  | INSTRUCTION   of Instr.instr
  | FILL_ARRAY    of fill_array_data
  | SWITCH        of switch

(** [map_item] format for [map_list], which appears in the [d_data] *)
and map_item = {
  type_of_item    : type_code;
  (* unsigned short padding *)
  mi_size         : int;
  mi_offset       : link;
}

(** type of the items, used at {!map_item} *)
and type_code =
  | TYPE_HEADER_ITEM                 (** 0x0000 *)
  | TYPE_STRING_ITEM                 (** 0x0001 *)
  | TYPE_TYPE_ID_ITEM                (** 0x0002 *)
  | TYPE_PROTO_ID_ITEM               (** 0x0003 *)
  | TYPE_FIELD_ID_ITEM               (** 0x0004 *)
  | TYPE_METHOD_ID_ITEM              (** 0x0005 *)
  | TYPE_CLASS_DEF_ITEM              (** 0x0006 *)
  | TYPE_MAP_LIST                    (** 0x1000 *)
  | TYPE_TYPE_LIST                   (** 0x1001 *)
  | TYPE_ANNOTATION_SET_REF_LIST     (** 0x1002 *)
  | TYPE_ANNOTATION_SET_ITEM         (** 0x1003 *)
  | TYPE_CLASS_DATA_ITEM             (** 0x2000 *)
  | TYPE_CODE_ITEM                   (** 0x2001 *)
  | TYPE_STRING_DATA_ITEM            (** 0x2002 *)
  | TYPE_DEBUG_INFO_ITEM             (** 0x2003 *)
  | TYPE_ANNOTATION_ITEM             (** 0x2004 *)
  | TYPE_ENCODED_ARRAY_ITEM          (** 0x2005 *)
  | TYPE_ANNOTATION_DIRECTORY_ITEM   (** 0x2006 *)

(** {!class_data_item} referenced from {!class_def_item} *)
and class_data_item = {
  mutable static_fields   : encoded_field  list;
  mutable instance_fields : encoded_field  list;
  mutable direct_methods  : encoded_method list;
  mutable virtual_methods : encoded_method list;
}

(** {!encoded_field} format used at {!class_data_item} *)
and encoded_field = {
  field_idx       : link;
  f_access_flag   : int;
}

(** {!encoded_method} format used at {!class_data_item} *)
and encoded_method = {
  method_idx      : link;
  mutable m_access_flag : int;
  code_off        : link;
}

(** {!code_item} referenced from {!encoded_method} *)
and code_item = {
  mutable registers_size : int;
  mutable ins_size       : int;
  mutable outs_size      : int;
  mutable tries_size     : int;
  mutable debug_info_off : link;
  mutable insns_size     : int;
          insns          : link DynArray.t;
  (* unsigned short padding *)
  mutable tries          : try_item list;
  mutable c_handlers     : encoded_catch_handler list;
}

(** [packed-switch] and [sparse-switch] format in [insns] of {!code_item} *)
and switch = {
  mutable sw_base : link;
  sw_size         : int;
  sw_keys         : int list;
  sw_targets      : link list;
}

(** [fill-array-data] format in [insns] of {!code_item} *)
and fill_array_data = {
  ad_width        : int;
  ad_size         : int;
  ad_data         : Instr.operand list;
}

(** {!try_item} format referenced from {!code_item} *)
and try_item = {
  start_addr      : link;
  end_addr        : link;
  handler_off     : link;
}

(** {!encoded_catch_handler} format referenced from {!code_item} *)
and encoded_catch_handler = {
  e_handlers      : type_addr_pair list;
  catch_all_addr  : link;
}

(** [encoded_type_addr_pair] format referenced from {!encoded_catch_handler} *)
and type_addr_pair = {
  mutable ch_type_idx : link;
  addr            : link;
}

(** {!debug_info_item} referenced from {!code_item} *)
and debug_info_item = {
  line_start      : int;
  parameter_name  : link list;
  mutable state_machine : (state_machine_instr * Instr.operand list) list;
}

(** byte code values for [state_machine] inside {!debug_info_item} *)
and state_machine_instr =
  | DBG_END_SEQUENCE            (** 0x00 *)
  | DBG_ADVANCE_PC              (** 0x01 *)
  | DBG_ADVANCE_LINE            (** 0x02 *)
  | DBG_START_LOCAL             (** 0x03 *)
  | DBG_START_LOCAL_EXTENDED    (** 0x04 *)
  | DBG_END_LOCAL               (** 0x05 *)
  | DBG_RESTART_LOCAL           (** 0x06 *)
  | DBG_SET_PROLOGUE_END        (** 0x07 *)
  | DBG_SET_EPILOGUE_BEGIN      (** 0x08 *)
  | DBG_SET_FILE                (** 0x09 *)
  | DBG_SPECIAL                 (** 0x0a..0xff *)

(** [annotations_directory_item] referenced from {!class_def_item} *)
and anno_dir_item = {
  class_anno_off  : link;
  fields          : anno_off list;
  methods         : anno_off list;
  parameters      : anno_off list;
}

(** [(field|method|parameter)_annotation] format used at {!anno_dir_item} *)
and anno_off = {
  target          : link;
  annotation_off  : link;
}

(** {!annotation_item} referenced from [ANNO_SET] *)
and annotation_item = {
  visible         : visibility;
  annotation      : encoded_annotation;
}

(** [Visibility] values *)
and visibility =
  | VISIBILITY_BUILD            (** 0x00 *)
  | VISIBILITY_RUNTIME          (** 0x01 *)
  | VISIBILITY_SYSTEM           (** 0x02 *)

(** {!encoded_annotation} format referenced from {!encoded_value} *)
and encoded_annotation = {
  mutable an_type_idx : link;
  elements        : annotation_element list;
}

(** {!annotation_element} format referenced from {!encoded_annotation} *)
and annotation_element = {
  name_idx        : link;
  mutable value : encoded_value;
}

(** {!encoded_value} encoding
 embedded in {!annotation_element} and [encoded_array] *)
and encoded_value =
  | VALUE_BYTE       of int64                (** 0x00 *)
  | VALUE_SHORT      of int64                (** 0x02 *)
  | VALUE_CHAR       of int64                (** 0x03 *)
  | VALUE_INT        of int64                (** 0x04 *)
  | VALUE_LONG       of int64                (** 0x06 *)
  | VALUE_FLOAT      of int64                (** 0x10 *)
  | VALUE_DOUBLE     of int64                (** 0x11 *)
  | VALUE_STRING     of int                  (** 0x17 *)
  | VALUE_TYPE       of int                  (** 0x18 *)
  | VALUE_FIELD      of int                  (** 0x19 *)
  | VALUE_METHOD     of int                  (** 0x1a *)
  | VALUE_ENUM       of int                  (** 0x1b *)
  | VALUE_ARRAY      of encoded_value list   (** 0x1c *)
  | VALUE_ANNOTATION of encoded_annotation   (** 0x1d *)
  | VALUE_NULL                               (** 0x1e *)
  | VALUE_BOOLEAN    of bool                 (** 0x1f *)

(** {2 Utilities} *)

(** wrapping with [Idx] *)
val to_idx : int -> link

(** wrapping with [Off] *)
val to_off : int -> link

(** unwrapping [Idx] *)
val of_idx : link -> int

(** unwrapping [Off] *)
val of_off : link -> int

module IdxKey :
sig
  type t = link
  val compare : t -> t -> int
end

module OffKey :
sig
  type t = link
  val compare : t -> t -> int
end

(** from [OPR_INDEX] to [Idx] *)
val opr2idx : Instr.operand -> link

(** from [OPR_OFFSET] to [Off] *)
val opr2off : Instr.operand -> link

(** from [Idx] to [OPR_INDEX] *)
val idx2opr : link -> Instr.operand

(** from [Off] to [OPR_OFFSET] *)
val off2opr : link -> Instr.operand

(** obtain 32-bits offset from [Off] *)
val get_off : link -> Instr.offset

(** obtain {!endian} from [string] representation *)
val str_to_endian : string -> endian

(** [string] representation of {!endian} *)
val endian_to_str : endian -> string

(** convert [int] to corresponding {!type_code} *)
val to_type_code : int -> type_code

(** get [int] value of given {!type_code} *)
val of_type_code : type_code -> int

(** get [string] notation of given {!type_code} *)
val type_code_to_str : type_code -> string

(** get [string] notation of given {!state_machine_instr} *)
val machine_instr_to_str : state_machine_instr -> string

(** {2 Access flags} *)

(** indicate the accessibility *)
type access_flag =
  | ACC_PUBLIC                (** 0x1, for all kinds *)
  | ACC_PRIVATE               (** 0x2, for all kinds *)
  | ACC_PROTECTED             (** 0x4, for all kinds *)
  | ACC_STATIC                (** 0x8, for all kinds *)
  | ACC_FINAL                 (** 0x10, for all kinds *)
  | ACC_SYNCHRONIZED          (** 0x20, only for methods *)
  | ACC_VOLATILE              (** 0x40, only for fields *)
  | ACC_BRIDGE                (** 0x40, only for methods *)
  | ACC_TRANSIENT             (** 0x80, only for fields *)
  | ACC_VARARGS               (** 0x80, only for methods *)
  | ACC_NATIVE                (** 0x100, only for methods *)
  | ACC_INTERFACE             (** 0x200, only for classes *)
  | ACC_ABSTRACT              (** 0x400, except for fields *)
  | ACC_STRICT                (** 0x800, only for methods *)
  | ACC_SYNTHETIC             (** 0x1000, for all kinds *)
  | ACC_ANNOTATION            (** 0x2000, only for classes *)
  | ACC_ENUM                  (** 0x4000, except for methods *)
  | ACC_CONSTRUCTOR           (** 0x10000, only for methods *)
  | ACC_DECLARED_SYNCHRONIZED (** 0x20000, only for methods *)

(** distinguish targets for {!access_flag} *)
type acc_kind =
  | ACC_FOR_CLASSES
  | ACC_FOR_FIELDS
  | ACC_FOR_METHODS

(** make [int] representation from bitfields of {!access_flag} *)
val to_acc_flag : acc_kind -> access_flag list -> int

(** check certain flags are set *)
val chk_acc_flag : acc_kind -> access_flag list -> int -> bool

(** [true] if [ACC_STATIC] is set *)
val is_static : int -> bool

(** [true] if [ACC_INTERFACE] is set *)
val is_interface : int -> bool

(** [true] if [ACC_SYNTHETIC] is set *)
val is_synthetic : int -> bool

(** [ACC_FOR_PUBLIC] *)
val pub : access_flag list

(** [ACC_STATIC] along with {!pub} *)
val spub : access_flag list

(** {2 Navigation} *)

(** 0xffffffff (= -1 if signed int) *)
val no_index : int

(** 0x00000000 *)
val no_offset : int

(** wrapping {!no_index} with [Idx] *)
val no_idx : link

(** wrapping {!no_offset} with [Off] *)
val no_off : link

(** get {!data_item} for given offset *)
val get_data_item : dex -> link -> data_item

(** get {!Instr.instr} for given offset,
 raise {!Wrong_match} unless [INSTRUCTION] *)
val get_ins : dex -> link -> Instr.instr

(** [true] if the item for given offset is {!Instr.instr} *)
val is_ins : dex -> link -> bool

(** get [string] for given string id,
 raise {!Wrong_match} unless [STRING_DATA] *)
val get_str : dex -> link -> string

(** find string id for given [string],
 {!no_idx} unless found *)
val find_str : dex -> string -> link

(** get type name for given type id *)
val get_ty_str : dex -> link -> string

(** find type id for given [string],
 {!no_idx} unless found *)
val find_ty_str : dex -> string -> link

(** comparator for type ids *)
val ty_comp : dex -> link -> link -> int

(** get [TYPE_LIST] for given offset,
 raise {!Wrong_match} unless [TYPE_LIST] *)
val get_ty_lst : dex -> link -> link list

(** get {!field_id_item} for given field id *)
val get_fit : dex -> link -> field_id_item

(** get {!method_id_item} for given method id *)
val get_mit : dex -> link -> method_id_item

(** get {!proto_id_item} for a given method. *)
val get_pit : dex -> method_id_item -> proto_id_item

(** get type for given field *)
val get_fty : dex -> field_id_item -> link

(** get a [list] of arguments for given method *)
val get_argv : dex -> method_id_item -> link list

(** get return type for given method *)
val get_rety : dex -> method_id_item -> link

(** comparator for field signatures: field name and type *)
val fld_comp : dex -> field_id_item -> field_id_item -> int

(** comparator for a [list] of type ids *)
val ty_lst_comp : dex -> link list -> link list -> int

(** comparator for a [list] of type ids,
  but ignore the package name for types. *)
val ty_lst_comp_relaxed : dex -> link list -> link list -> int

(** comparator for method signatures: method name, return type, and arguments *)
val mtd_comp : dex -> method_id_item -> method_id_item -> int

(** comparator for method signatures: method name, return type, and arguments,
  but ignore the package name for return types and arguments. *)
val mtd_comp_relaxed : dex -> method_id_item -> method_id_item -> int

(** get class id from field id *)
val get_cid_from_fid : dex -> link -> link

(** get class id from method id *)
val get_cid_from_mid : dex -> link -> link

(** get name for given field *)
val get_fld_name : dex -> link -> string

(** get name for given method *)
val get_mtd_name : dex -> link -> string

(** get name for given field, along with class name *)
val get_fld_full_name : dex -> link -> string

(** get name for given method, along with class name *)
val get_mtd_full_name : dex -> link -> string

(** get method signature, e.g., [Lpkg/cls;->mtd(arg1;arg2;...)rety] *)
val get_mtd_sig : dex -> link -> string

(** get class id from name, {!no_idx} unless found *)
val get_cid : dex -> string -> link

(** get {!class_def_item} for given class id,
 raise [Not_found] unless found *)
val get_cdef : dex -> link -> class_def_item

(** get interfaces implemented by the given class *)
val get_interfaces : dex -> link -> link list

(** get classes that implement the given interface *)
val get_implementers : dex -> link -> link list

(** get super class id for given class,
 {!no_idx} if it's at the top level *)
val get_superclass : dex -> link -> link

(** get super classes for a given class *)
val get_superclasses : dex -> link -> link list

(** check that some property (given as a function {!link} to [bool])
 holds in hierarchy starting from the given class *)
val in_hierarchy : dex -> (link -> bool) -> link -> bool

(** check whether some class is a super class (up through the hierarchy)
    of a given class *)
val is_superclass : dex -> link -> link -> bool

(** check whether some class is an inner class of the given class *)
val is_innerclass : dex -> link -> link -> bool

(** get inner classes for the given class *)
val get_innerclasses : dex -> link -> link list

(** get owning class if the given class is an inner class *)
val get_owning_class : dex -> link -> link

(** get all fields, along with ids, for given class *)
val get_flds : dex -> link -> (link * field_id_item) list

(** get all fields, along with ids, for given class and superclasses *)
val get_fldS : dex -> link -> (link * field_id_item) list

(** get the specific field of given class and given field name *)
val get_the_fld : dex -> link -> string -> link * field_id_item

(** [true] if the class owns the field *)
val own_the_fld : dex -> link -> link -> bool

(** get all methods, along with ids, for given class *)
val get_mtds : dex -> link -> (link * method_id_item) list

(** get all methods, along with ids, for given class and superclasses *)
val get_mtdS : dex -> link -> (link * method_id_item) list

(** get overriden method at the superclass for given class and method,
 {!no_idx} unless overridable *)
val get_supermethod : dex -> link -> link -> link

(** get the specific method of given class and given method name *)
val get_the_mtd : dex -> link -> string -> link * method_id_item

(** get the specific method of given class, method name,
 and shorty descriptor (useful for overloading) *)
val get_the_mtd_shorty : dex -> link -> string -> string -> link * method_id_item

(** [true] if the class owns the method *)
val own_the_mtd : dex -> link -> link -> bool

(** get {!class_data_item} for given class,
 raise {!Wrong_match} unless [CLASS_DATA] *)
val get_cdata : dex -> link -> link * class_data_item

(** get static fields for given class, along with initial values if exists *)
val get_stt_flds : dex -> link -> (link * encoded_value option) list

(** get {!encoded_method} for given class and method,
 raise {!Wrong_dex} if such method is not defined *)
val get_emtd : dex -> link -> link -> encoded_method

(** get {!code_item} for given class and method,
 raise {!Wrong_match} unless [CODE_ITEM] *)
val get_citm : dex -> link -> link -> link * code_item

(** calculate a register number that holds [this] pointer *)
val calc_this : code_item -> int

(** [true] if the given register is used as a parameter *)
val is_param : code_item -> int -> bool

(** {2 Modification helper} *)

(** empty {!section} *)
val empty_section : unit -> section

(** empty {!dex} *)
val empty_dex : unit -> dex

(** empty {!code_item} *)
val empty_citm : unit -> code_item

(** insert {!data_item} into the data pool *)
val insrt_data : dex -> link -> data_item -> unit

(** remove {!data_item} in the data pool *)
val rm_data : dex -> link -> unit

(** insert {!Instr.instr} into the data pool *)
val insrt_ins : dex -> link -> Instr.instr -> unit

(** insert [string] into the data pool *)
val insrt_str : dex -> link -> string -> unit

(** insert [TYPE_LIST] into the data pool *)
val insrt_ty_lst : dex -> link -> link list -> unit

(** insert [STATIC_VALUE] into the data pool *)
val insrt_stt : dex -> link -> encoded_value list -> unit

(** insert {!code_item} into the data pool *)
val insrt_citm : dex -> link -> code_item -> unit

