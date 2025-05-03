(*
 * Copyright (c) 2010-2014,
 *  Jinseong Jeon <jsjeon@cs.umd.edu>
 *  Kris Micinski <micinski@cs.umd.edu>
 *  Jeff Foster   <jfoster@cs.umd.edu>
 * All rights reserved.
 *
 * Based on the src/dex.ml from https://github.com/plum-umd/redexer
 *)

(* Format reference is https://source.android.com/devices/tech/dalvik/dex-format.html
 *)

(***********************************************************************)
(* Dex                                                                 *)
(***********************************************************************)

module DA = DynArray

module I  = Dalvik_disasm

module J = Java

(***********************************************************************)
(* Basic Types                                                         *)
(***********************************************************************)

exception Wrong_dex of string
exception Wrong_match of string
exception No_return
exception NOT_YET of string

type dex = {
  header          : dex_header;
  d_string_ids    : link DA.t;
  d_type_ids      : link DA.t;
  d_proto_ids     : proto_id_item  DA.t;
  d_field_ids     : field_id_item  DA.t;
  d_method_ids    : method_id_item DA.t;
  d_class_defs    : class_def_item DA.t;
  mutable d_data  : data_item IM.t;
}

and link =
  | Idx of int
  | Off of I.offset

and dex_header = {
  magic           : string;
  checksum        : int64;
  signature       : char list;
  mutable file_size : int;
  header_size     : int;        (* usually, 0x70 *)
  endian_tag      : endian;     (* usually, ENDIAN_CONSTANT *)
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

and endian  =
  | LITTLE        (* ENDIAN_CONSTANT         = 0x12345678 *)
  | BIG           (* REVERSE_ENDIAN_CONSTANT = 0x78563412 *)

and section = {
  size            : int;
  offset          : link;
}

and proto_id_item = {
  shorty          : link;
  mutable return_type : link;
  parameter_off   : link;
}

and field_id_item = {
  f_class_id      : link;
  mutable f_type_id : link;
  f_name_id       : link;
}

and method_id_item = {
  m_class_id      : link;
  m_proto_id      : link;
  m_name_id       : link;
}

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

and data_item =
  | MAP_LIST      of map_item list
  | TYPE_LIST     of link list
  | ANNO_SET_REF  of link list
  | ANNO_SET      of link list
  | CLASS_DATA    of class_data_item
  | CODE_ITEM     of code_item
  | STRING_DATA   of UTF8.t
  | DEBUG_INFO    of debug_info_item
  | ANNOTATION    of annotation_item
  | STATIC_VALUE  of encoded_value list
  | ANNO_DIR      of anno_dir_item
  | INSTRUCTION   of I.instr
  | FILL_ARRAY    of fill_array_data
  | SWITCH        of switch

and map_item = {
  type_of_item    : type_code;
  (* unsigned short padding *)
  mi_size         : int;
  mi_offset       : link;
}

and type_code =
  | TYPE_HEADER_ITEM                 (* 0x0000 *)
  | TYPE_STRING_ITEM                 (* 0x0001 *)
  | TYPE_TYPE_ID_ITEM                (* 0x0002 *)
  | TYPE_PROTO_ID_ITEM               (* 0x0003 *)
  | TYPE_FIELD_ID_ITEM               (* 0x0004 *)
  | TYPE_METHOD_ID_ITEM              (* 0x0005 *)
  | TYPE_CLASS_DEF_ITEM              (* 0x0006 *)
  | TYPE_MAP_LIST                    (* 0x1000 *)
  | TYPE_TYPE_LIST                   (* 0x1001 *)
  | TYPE_ANNOTATION_SET_REF_LIST     (* 0x1002 *)
  | TYPE_ANNOTATION_SET_ITEM         (* 0x1003 *)
  | TYPE_CLASS_DATA_ITEM             (* 0x2000 *)
  | TYPE_CODE_ITEM                   (* 0x2001 *)
  | TYPE_STRING_DATA_ITEM            (* 0x2002 *)
  | TYPE_DEBUG_INFO_ITEM             (* 0x2003 *)
  | TYPE_ANNOTATION_ITEM             (* 0x2004 *)
  | TYPE_ENCODED_ARRAY_ITEM          (* 0x2005 *)
  | TYPE_ANNOTATION_DIRECTORY_ITEM   (* 0x2006 *)

and class_data_item = {
  mutable static_fields   : encoded_field  list;
  mutable instance_fields : encoded_field  list;
  mutable direct_methods  : encoded_method list;
  mutable virtual_methods : encoded_method list;
}

and encoded_field = {
  field_idx       : link;
  f_access_flag   : int;
}

and encoded_method = {
  method_idx      : link;
  mutable m_access_flag : int;
  code_off        : link;
}

and code_item = {
  mutable registers_size : int;
  mutable ins_size       : int;
  mutable outs_size      : int;
  mutable tries_size     : int;
  mutable debug_info_off : link;
  mutable insns_size     : int;
          insns          : link DA.t;
  (* unsigned short padding *)
  mutable tries          : try_item list;
  mutable c_handlers     : encoded_catch_handler list;
}

and switch = {
  mutable sw_base : link;
  sw_size         : int;
  sw_keys         : int list;
  sw_targets      : link list;
}

and fill_array_data = {
  ad_width        : int;
  ad_size         : int;
  ad_data         : I.operand list;
}

and try_item = {
  start_addr      : link;
  end_addr        : link;
  handler_off     : link;
}

and encoded_catch_handler = {
  e_handlers      : type_addr_pair list;
  catch_all_addr  : link;
}

and type_addr_pair = {
  mutable ch_type_idx : link;
  addr            : link;
}

and debug_info_item = {
  line_start      : int;
  parameter_name  : link list;
  mutable state_machine : (state_machine_instr * I.operand list) list;
}

and state_machine_instr =
  | DBG_END_SEQUENCE            (* 0x00 *)
  | DBG_ADVANCE_PC              (* 0x01 *)
  | DBG_ADVANCE_LINE            (* 0x02 *)
  | DBG_START_LOCAL             (* 0x03 *)
  | DBG_START_LOCAL_EXTENDED    (* 0x04 *)
  | DBG_END_LOCAL               (* 0x05 *)
  | DBG_RESTART_LOCAL           (* 0x06 *)
  | DBG_SET_PROLOGUE_END        (* 0x07 *)
  | DBG_SET_EPILOGUE_BEGIN      (* 0x08 *)
  | DBG_SET_FILE                (* 0x09 *)
  | DBG_SPECIAL                 (* 0x0a..0xff *)

and anno_dir_item = {
  class_anno_off  : link;
  fields          : anno_off list;
  methods         : anno_off list;
  parameters      : anno_off list;
}

and anno_off = {
  target          : link;
  annotation_off  : link;
}

and annotation_item = {
  visible         : visibility;
  annotation      : encoded_annotation;
}

and visibility =
  | VISIBILITY_BUILD            (* 0x00 *)
  | VISIBILITY_RUNTIME          (* 0x01 *)
  | VISIBILITY_SYSTEM           (* 0x02 *)

and encoded_annotation = {
  mutable an_type_idx : link;
  elements        : annotation_element list;
}

and annotation_element = {
  name_idx        : link;
  mutable value : encoded_value;
}

and encoded_value =
  | VALUE_BYTE       of int64                (* 0x00 *)
  | VALUE_SHORT      of int64                (* 0x02 *)
  | VALUE_CHAR       of int64                (* 0x03 *)
  | VALUE_INT        of int64                (* 0x04 *)
  | VALUE_LONG       of int64                (* 0x06 *)
  | VALUE_FLOAT      of int64                (* 0x10 *)
  | VALUE_DOUBLE     of int64                (* 0x11 *)
  | VALUE_STRING     of int                  (* 0x17 *)
  | VALUE_TYPE       of int                  (* 0x18 *)
  | VALUE_FIELD      of int                  (* 0x19 *)
  | VALUE_METHOD     of int                  (* 0x1a *)
  | VALUE_ENUM       of int                  (* 0x1b *)
  | VALUE_ARRAY      of encoded_value list   (* 0x1c *)
  | VALUE_ANNOTATION of encoded_annotation   (* 0x1d *)
  | VALUE_NULL                               (* 0x1e *)
  | VALUE_BOOLEAN    of bool                 (* 0x1f *)

(***********************************************************************)
(* Utilities                                                           *)
(***********************************************************************)

exception Wrong_link of string

(* to_idx : int -> link *)
let to_idx (i: int) : link = Idx i

(* to_off : int -> link *)
let to_off (i: int) : link = Off (Int32.of_int i)

(* of_idx : link -> int *)
let of_idx (l: link) : int =
  match l with
  | Idx idx -> idx
  | _ -> raise (Wrong_link "of_idx")

(* of_off : link -> int *)
let of_off (l: link) : int =
  match l with
  | Off off -> Int32.to_int off
  | _ -> raise (Wrong_link "of_off")

module IdxKey =
struct
  type t = link
  let compare id1 id2 = Pervasives.compare (of_idx id1) (of_idx id2)
end

module OffKey =
struct
  type t = link
  let compare o1 o2 = Pervasives.compare (of_off o1) (of_off o2)
end

(* opr2idx : I.operand -> link *)
let opr2idx (opr: I.operand) : link =
  match opr with
  | I.OPR_INDEX i -> to_idx i
  | _ -> raise (Wrong_match "opr2idx")

(* opr2off : I.operand -> link *)
let opr2off (opr: I.operand) : link =
  match opr with
  | I.OPR_OFFSET i32 -> Off i32
  | _ -> raise (Wrong_match "opr2off")

(* idx2opr : link -> I.operand *)
let idx2opr (l: link) : I.operand =
  match l with
  | Idx idx -> I.OPR_INDEX idx
  | _ -> raise (Wrong_match "idx2opr")

(* off2opr : link -> I.operand *)
let off2opr (l: link) : I.operand =
  match l with
  | Off off -> I.OPR_OFFSET off
  | _ -> raise (Wrong_match "off2opr")

(* get_off : link -> I.offset *)
let get_off (l: link) : I.offset =
  match l with
  | Off off -> off
  | _ -> raise (Wrong_link "get_off")

(* str_to_endian : string -> endian *)
let str_to_endian (str: string) : endian =
  if (str = "0x12345678L") then LITTLE
  else if (str = "0x78563412L") then BIG
  else raise (Wrong_dex "str_to_endian")

(* endian_to_str : endian -> string *)
let endian_to_str (e: endian) : string =
  match e with
  | LITTLE -> "0x12345678L"
  | BIG    -> "0x78563412L"

(* to_type_code : int -> type_code *)
let to_type_code (i: int) : type_code =
  match i with
  | 0x0000 -> TYPE_HEADER_ITEM
  | 0x0001 -> TYPE_STRING_ITEM
  | 0x0002 -> TYPE_TYPE_ID_ITEM
  | 0x0003 -> TYPE_PROTO_ID_ITEM
  | 0x0004 -> TYPE_FIELD_ID_ITEM
  | 0x0005 -> TYPE_METHOD_ID_ITEM
  | 0x0006 -> TYPE_CLASS_DEF_ITEM
  | 0x1000 -> TYPE_MAP_LIST
  | 0x1001 -> TYPE_TYPE_LIST
  | 0x1002 -> TYPE_ANNOTATION_SET_REF_LIST
  | 0x1003 -> TYPE_ANNOTATION_SET_ITEM
  | 0x2000 -> TYPE_CLASS_DATA_ITEM
  | 0x2001 -> TYPE_CODE_ITEM
  | 0x2002 -> TYPE_STRING_DATA_ITEM
  | 0x2003 -> TYPE_DEBUG_INFO_ITEM
  | 0x2004 -> TYPE_ANNOTATION_ITEM
  | 0x2005 -> TYPE_ENCODED_ARRAY_ITEM
  | 0x2006 -> TYPE_ANNOTATION_DIRECTORY_ITEM
  | _ -> raise (Wrong_dex ("illegal type code: "^(string_of_int i)))

(* of_type_code : type_code -> int *)
let of_type_code (t: type_code) : int =
  match t with
  | TYPE_HEADER_ITEM               -> 0x0000
  | TYPE_STRING_ITEM               -> 0x0001
  | TYPE_TYPE_ID_ITEM              -> 0x0002
  | TYPE_PROTO_ID_ITEM             -> 0x0003
  | TYPE_FIELD_ID_ITEM             -> 0x0004
  | TYPE_METHOD_ID_ITEM            -> 0x0005
  | TYPE_CLASS_DEF_ITEM            -> 0x0006
  | TYPE_MAP_LIST                  -> 0x1000
  | TYPE_TYPE_LIST                 -> 0x1001
  | TYPE_ANNOTATION_SET_REF_LIST   -> 0x1002
  | TYPE_ANNOTATION_SET_ITEM       -> 0x1003
  | TYPE_CLASS_DATA_ITEM           -> 0x2000
  | TYPE_CODE_ITEM                 -> 0x2001
  | TYPE_STRING_DATA_ITEM          -> 0x2002
  | TYPE_DEBUG_INFO_ITEM           -> 0x2003
  | TYPE_ANNOTATION_ITEM           -> 0x2004
  | TYPE_ENCODED_ARRAY_ITEM        -> 0x2005
  | TYPE_ANNOTATION_DIRECTORY_ITEM -> 0x2006

(* type_code_to_str : type_code -> string *)
let type_code_to_str (tc: type_code) : string =
  match tc with
  | TYPE_HEADER_ITEM               -> "HDR_ITM"
  | TYPE_STRING_ITEM               -> "STR_ITM"
  | TYPE_TYPE_ID_ITEM              -> "TYP_ITM"
  | TYPE_PROTO_ID_ITEM             -> "PRT_ITM"
  | TYPE_FIELD_ID_ITEM             -> "FLD_ITM"
  | TYPE_METHOD_ID_ITEM            -> "MTD_ITM"
  | TYPE_CLASS_DEF_ITEM            -> "CLS_DEF"
  | TYPE_MAP_LIST                  -> "MAP_LST"
  | TYPE_TYPE_LIST                 -> "TYP_LST"
  | TYPE_ANNOTATION_SET_REF_LIST   -> "ANN_REF"
  | TYPE_ANNOTATION_SET_ITEM       -> "ANN_SET"
  | TYPE_CLASS_DATA_ITEM           -> "CLS_ITM"
  | TYPE_CODE_ITEM                 -> "COD_ITM"
  | TYPE_STRING_DATA_ITEM          -> "STR_DAT"
  | TYPE_DEBUG_INFO_ITEM           -> "DBG_INF"
  | TYPE_ANNOTATION_ITEM           -> "ANN_ITM"
  | TYPE_ENCODED_ARRAY_ITEM        -> "ENC_ARR"
  | TYPE_ANNOTATION_DIRECTORY_ITEM -> "ANN_DIR"

(* machine_instr_to_str : state_machine_instr -> string *)
let machine_instr_to_str (op: state_machine_instr) : string =
  match op with
  | DBG_END_SEQUENCE         -> "0x00"
  | DBG_ADVANCE_PC           -> "0x01"
  | DBG_ADVANCE_LINE         -> "0x02"
  | DBG_START_LOCAL          -> "0x03"
  | DBG_START_LOCAL_EXTENDED -> "0x04"
  | DBG_END_LOCAL            -> "0x05"
  | DBG_RESTART_LOCAL        -> "0x06"
  | DBG_SET_PROLOGUE_END     -> "0x07"
  | DBG_SET_EPILOGUE_BEGIN   -> "0x08"
  | DBG_SET_FILE             -> "0x09"
  | DBG_SPECIAL              -> "0x??"

(***********************************************************************)
(* Access flags                                                        *)
(***********************************************************************)

type access_flag =
  | ACC_PUBLIC
  | ACC_PRIVATE
  | ACC_PROTECTED
  | ACC_STATIC
  | ACC_FINAL
  | ACC_SYNCHRONIZED
  | ACC_VOLATILE
  | ACC_BRIDGE
  | ACC_TRANSIENT
  | ACC_VARARGS
  | ACC_NATIVE
  | ACC_INTERFACE
  | ACC_ABSTRACT
  | ACC_STRICT
  | ACC_SYNTHETIC
  | ACC_ANNOTATION
  | ACC_ENUM
  | ACC_CONSTRUCTOR
  | ACC_DECLARED_SYNCHRONIZED

type acc_kind =
  | ACC_FOR_CLASSES
  | ACC_FOR_FIELDS
  | ACC_FOR_METHODS

(* to_acc_flag : acc_kind -> access_flag list -> int *)
let rec to_acc_flag (k: acc_kind) = function
  | [] -> 0
  | h::t ->
  (
    let v =
      match k, h with
      |               _, ACC_PUBLIC                -> 0x1
      |               _, ACC_PRIVATE               -> 0x2
      |               _, ACC_PROTECTED             -> 0x4
      |               _, ACC_STATIC                -> 0x8
      |               _, ACC_FINAL                 -> 0x10
      | ACC_FOR_METHODS, ACC_SYNCHRONIZED          -> 0x20
      | ACC_FOR_FIELDS , ACC_VOLATILE              -> 0x40
      | ACC_FOR_METHODS, ACC_BRIDGE                -> 0x40
      | ACC_FOR_FIELDS , ACC_TRANSIENT             -> 0x80
      | ACC_FOR_METHODS, ACC_VARARGS               -> 0x80
      | ACC_FOR_METHODS, ACC_NATIVE                -> 0x100
      | ACC_FOR_CLASSES, ACC_INTERFACE             -> 0x200
      | ACC_FOR_CLASSES, ACC_ABSTRACT
      | ACC_FOR_METHODS, ACC_ABSTRACT              -> 0x400
      | ACC_FOR_METHODS, ACC_STRICT                -> 0x800
      |               _, ACC_SYNTHETIC             -> 0x1000
      | ACC_FOR_CLASSES, ACC_ANNOTATION            -> 0x2000
      | ACC_FOR_CLASSES, ACC_ENUM
      | ACC_FOR_FIELDS , ACC_ENUM                  -> 0x4000
      | ACC_FOR_METHODS, ACC_CONSTRUCTOR           -> 0x10000
      | ACC_FOR_METHODS, ACC_DECLARED_SYNCHRONIZED -> 0x20000
    in
    v lor (to_acc_flag k t)
  )

(* chk_acc_flag : acc_kind -> access_flag list -> int -> bool *)
let chk_acc_flag (k: acc_kind) (flags: access_flag list) (flag: int) : bool =
  0 <> flag land (to_acc_flag k flags)

(* is_static : int -> bool *)
let is_static (flag: int) : bool =
  chk_acc_flag ACC_FOR_METHODS [ACC_STATIC] flag

(* is_interface : int -> bool *)
let is_interface (flag: int) : bool =
  chk_acc_flag ACC_FOR_CLASSES [ACC_INTERFACE] flag

(* is_synthetic : int -> bool *)
let is_synthetic (flag: int) : bool =
  chk_acc_flag ACC_FOR_METHODS [ACC_SYNTHETIC] flag

(* pub : access_flag list *)
let pub = [ACC_PUBLIC]

(* spub : access_flag list *)
let spub = ACC_STATIC :: pub

(***********************************************************************)
(* DEX Navigation                                                      *)
(***********************************************************************)

let no_index = -1 (* NO_INDEX = 0xffffffff (= -1 if signed int) *)
let no_offset = 0

let no_idx = to_idx no_index
let no_off = to_off no_offset

(* get_data_item : dex -> link -> data_item *)
let get_data_item dx (l: link) : data_item =
  let off = get_off l in
  try IM.find off dx.d_data with Not_found ->
  let hx = Printf.sprintf "0x%08X" (Int32.to_int off) in
  raise (Wrong_dex ("get_data_item, no such offset: "^hx))

(* get_ins : dex -> link -> I.instr *)
let get_ins dx (l: link) : I.instr =
  match get_data_item dx l with
  | INSTRUCTION ins -> ins
  | _ -> raise (Wrong_match "get_ins")

(* is_ins : dex -> link -> bool *)
let is_ins dx (l: link) : bool =
  try ignore (get_ins dx l); true with _ -> false

(* get_str : dex -> link -> string *)
let get_str dx (sid: link) : string =
  let ditm = get_data_item dx (DA.get dx.d_string_ids (of_idx sid)) in
  match ditm with
  | STRING_DATA str -> str
  | _ -> raise (Wrong_match "get_str")

(* find_str : dex -> string -> link *)
let find_str dx (str: string) : link =
  let finder off : bool =
    match get_data_item dx off with
    | STRING_DATA str' when 0 = String.compare str' str -> true
    | _ -> false
  in
  try to_idx (DA.index_of finder dx.d_string_ids)
  with Not_found -> no_idx

(* get_ty_str : dex -> link -> string *)
let get_ty_str dx (tid: link) : string =
  if tid = no_idx then "" else
  get_str dx (DA.get dx.d_type_ids (of_idx tid))

(* find_ty_str : dex -> string -> link *)
let find_ty_str dx (str: string) : link =
  let finder sid : bool =
    0 = String.compare str (get_str dx sid)
  in
  try to_idx (DA.index_of finder dx.d_type_ids)
  with Not_found -> no_idx

let ty_comp_possibly_relaxed dx tid1 tid2 r =
  let s1 = get_ty_str dx tid1
  and s2 = get_ty_str dx tid2 in
  if r then
    try
      let c1 = J.get_class_name s1
      and c2 = J.get_class_name s2
      in String.compare c1 c2
    with _ -> String.compare s1 s2
  else
    String.compare s1 s2

(* ty_comp : dex -> link -> link -> int *)
let ty_comp dx (tid1: link) (tid2: link) : int =
  ty_comp_possibly_relaxed dx tid1 tid2 false

(* get_ty_lst : dex -> link -> link list *)
let get_ty_lst dx (off: link) : link list =
  if off = no_off then [] else
  match get_data_item dx off with
  | TYPE_LIST tl -> tl
  | _ -> raise (Wrong_match "get_ty_lst")

(* get_fit : dex -> link -> field_id_item *)
let get_fit dx (fid: link) : field_id_item =
  DA.get dx.d_field_ids (of_idx fid)

(* get_mit : dex -> link -> method_id_item *)
let get_mit dx (mid: link) : method_id_item =
  DA.get dx.d_method_ids (of_idx mid)

(* get_pit : dex -> method_id_item -> proto_id_item *)
let get_pit dx (mit: method_id_item) : proto_id_item =
  DA.get dx.d_proto_ids (of_idx mit.m_proto_id)

(* get_fty : dex -> field_id_item -> link *)
let get_fty dx (fit: field_id_item) : link =
  fit.f_type_id

(* get_argv : dex -> method_id_item -> link list *)
let get_argv dx (mit: method_id_item) : link list =
  let pit = get_pit dx mit in
  get_ty_lst dx pit.parameter_off

(* get_rety : dex -> method_id_item -> link *)
let get_rety dx (mit: method_id_item) : link =
  (get_pit dx mit).return_type

(* fld_comp : dex -> field_id_item -> field_id_item -> int *)
let fld_comp dx fit1 fit2 : int =
  let fname1 = get_str dx fit1.f_name_id
  and fname2 = get_str dx fit2.f_name_id in
  let c1 = compare fname1 fname2 in
  if c1 <> 0 then c1 else ty_comp dx fit1.f_type_id fit2.f_type_id

let rec ty_lst_comp_possibly_relaxed dx l1 l2 r =
  match l1, l2 with
  | [], [] -> 0 | [], _ -> -1 | _, [] -> 1
  | h1::t1, h2::t2 ->
    let c = ty_comp_possibly_relaxed dx h1 h2 r in
    if c <> 0 then c else ty_lst_comp_possibly_relaxed dx t1 t2 r

(* ty_lst_comp : dex -> link list -> link list -> int *)
let ty_lst_comp dx (l1: link list) (l2: link list) : int =
  ty_lst_comp_possibly_relaxed dx l1 l2 false

(* ty_lst_comp_relaxed : dex -> link list -> link list -> int *)
let ty_lst_comp_relaxed dx (l1: link list) (l2: link list) : int =
  ty_lst_comp_possibly_relaxed dx l1 l2 true

let mtd_comp_possibly_relaxed dx mit1 mit2 r : int =
  let mname1 = get_str dx mit1.m_name_id
  and mname2 = get_str dx mit2.m_name_id in
  let c1 = compare mname1 mname2 in
  if c1 <> 0 then c1 else
    let rety1 = get_rety dx mit1
    and rety2 = get_rety dx mit2
    and argv1 = get_argv dx mit1
    and argv2 = get_argv dx mit2 in
    ty_lst_comp_possibly_relaxed dx (rety1::argv1) (rety2::argv2) r

(* mtd_comp : dex -> method_id_item -> method_id_item -> int *)
let mtd_comp dx mit1 mit2 : int =
  mtd_comp_possibly_relaxed dx mit1 mit2 false

(* mtd_comp_relaxed : dex -> method_id_item -> method_id_item -> int *)
let mtd_comp_relaxed dx mit1 mit2 : int =
  mtd_comp_possibly_relaxed dx mit1 mit2 true

(* get_cid_from_fid : dex -> link -> link *)
let get_cid_from_fid dx (fid: link) : link =
  (get_fit dx fid).f_class_id

(* get_cid_from_mid : dex -> link -> link *)
let get_cid_from_mid dx (mid: link) : link =
  (get_mit dx mid).m_class_id

(* get_fld_name : dex -> link -> string *)
let get_fld_name dx (fid: link) : string =
  get_str dx (get_fit dx fid).f_name_id

(* get_mtd_name : dex -> link -> string *)
let get_mtd_name dx (mid: link) : string =
  get_str dx (get_mit dx mid).m_name_id

(* get_fld_full_name : dex -> link -> string *)
let get_fld_full_name dx (fid: link) : string =
  let cid = get_cid_from_fid dx fid in
  let cname = get_ty_str dx cid
  and fname = get_fld_name dx fid in
  cname^"."^fname

(* get_mtd_full_name : dex -> link -> string *)
let get_mtd_full_name dx (mid: link) : string =
  let cid = get_cid_from_mid dx mid in
  let cname = get_ty_str dx cid
  and mname = get_mtd_name dx mid in
  cname^"->"^mname

(* get_mtd_sig : dex -> link -> string *)
let get_mtd_sig dx (mid: link) : string =
  let mname = get_mtd_full_name dx mid
  and mit = get_mit dx mid in
  let argv = List.map (get_ty_str dx) (get_argv dx mit)
  and rety = get_ty_str dx (get_rety dx mit) in
  mname^"("^(List.fold_left (^) "" argv)^")"^rety

(* get_cid : dex -> string -> link *)
let get_cid dx (name: string) : link =
  find_ty_str dx name

(* get_cdef : dex -> link -> class_def_item *)
let get_cdef dx (cid: link) : class_def_item =
  let finder (cdef: class_def_item) : bool =
    cid = cdef.c_class_id
  in
  DA.get dx.d_class_defs (DA.index_of finder dx.d_class_defs)

let get_classes dx (f: link -> bool) : link list =
  let folder acc (cdef: class_def_item) =
    let cid = cdef.c_class_id in
    if f cid then cid :: acc else acc
  in
  DA.fold_left folder [] dx.d_class_defs

(* get_interfaces : dex -> link -> link list *)
let get_interfaces dx (cid: link) : link list =
  try get_ty_lst dx (get_cdef dx cid).interfaces
  with Not_found -> []

(* get_implementers : dex -> link -> link list *)
let get_implementers dx (cid: link) : link list =
  let f cid' =
    List.mem cid (get_interfaces dx cid')
  in
  get_classes dx f

(* get_superclass : dex -> link -> link *)
let get_superclass dx cid : link =
  try (get_cdef dx cid).superclass
  with Not_found -> no_idx

(* get_superclasses : dex -> link -> link list *)
let get_superclasses dx (cid: link) : link list =
  let rec h acc = function x ->
    if x = no_idx then acc
    else h (x::acc) (get_superclass dx x)
  in h [] cid

(* in_hierarchy : dex -> (link -> bool) -> link -> bool *)
let rec in_hierarchy dx (f: link -> bool) (cid: link) : bool =
  if cid = no_idx then false else
  if f cid then true else in_hierarchy dx f (get_superclass dx cid)

(* is_superclass : dex -> link -> link -> bool *)
let is_superclass dx (cid: link) (sup: link) : bool =
  in_hierarchy dx ((=) sup) cid

(* is_innerclass : dex -> link -> link -> bool *)
let is_innerclass dx (cid: link) (inner: link) : bool =
  let cname = get_ty_str dx cid
  and inner_name = get_ty_str dx inner in
  0 = String.compare cname (J.get_owning_class inner_name)

(* get_innerclasses : dex -> link -> link list *)
let get_innerclasses dx (cid: link) : link list =
  let f cid' =
    J.is_inner_class (get_ty_str dx cid')
    && is_innerclass dx cid cid'
  in
  get_classes dx f

(* get_owning_class : dex -> link -> link *)
let get_owning_class dx (cid: link) : link =
  let cname = get_ty_str dx cid in
  if not (J.is_inner_class cname) then no_idx else
    get_cid dx (J.get_owning_class cname)

(* get_flds : dex -> link -> (link * field_id_item) list *)
let get_flds dx (cid: link) : (link * field_id_item) list =
  let folder (id, acc) (fit: field_id_item) =
    let nid = id + 1 in
    if fit.f_class_id = cid
    then (nid, (to_idx id, fit)::acc) else (nid, acc)
  in
  snd (DA.fold_left folder (0,[]) dx.d_field_ids)

(* get_fldS : dex -> link -> (link * field_id_item) list *)
let get_fldS dx (cid: link) : (link * field_id_item) list =
  let rec collect_super cid' prv =
    let flds = get_flds dx cid' in
    let prv' = List.rev_append (L.rev flds) prv in
    let sid = get_superclass dx cid' in
    if sid <> no_idx then collect_super sid prv' else prv'
  in
  collect_super cid []

(* get_the_fld : dex -> link -> string -> link * field_id_item *)
let rec get_the_fld dx (cid: link) (fname: string) : link * field_id_item =
  let finder (cid': link) (fit: field_id_item) : bool =
    fit.f_class_id = cid' && get_str dx fit.f_name_id = fname
  in
  try
    let fid = DA.index_of (finder cid) dx.d_field_ids in
    to_idx fid, get_fit dx (to_idx fid)
  with Not_found ->
  (
    let sid = get_superclass dx cid in
    if sid <> no_idx then get_the_fld dx sid fname
    else raise (Wrong_dex ("get_the_fld: can't find the field: "^fname))
  )

(* own_the_fld : dex -> link -> link -> bool *)
let own_the_fld dx (cid: link) (fid: link) : bool =
  get_cid_from_fid dx fid = cid

(* get_mtds : dex -> link -> (link * method_id_item) list *)
let get_mtds dx (cid: link) : (link * method_id_item) list =
  let folder (id, acc) (mit: method_id_item) =
    let nid = id + 1 in
    if 0 = ty_comp dx mit.m_class_id cid
    then (nid, (to_idx id, mit)::acc) else (nid, acc)
  in
  snd (DA.fold_left folder (0,[]) dx.d_method_ids)

(* get_mtdS : dex -> link -> (link * method_id_item) list *)
let get_mtdS dx (cid: link) : (link * method_id_item) list =
  let rec collect_super cid' prv =
    let mtds = get_mtds dx cid' in
    let folder acc (id, mit) =
      let m_finder (_, mit') : bool =
        mtd_comp dx mit mit' = 0
      in (* to avoid redundancy *)
      try ignore (List.find m_finder acc); acc
      with Not_found -> (id, mit)::acc
    in
    let prv' = List.fold_left folder prv mtds in
    let sid = get_superclass dx cid' in
    if sid <> no_idx then collect_super sid prv' else prv'
  in
  collect_super cid []

(* get_supermethod: dex -> link -> link -> link *)
let rec get_supermethod dx (cid: link) (mid: link) : link =
  let mit = get_mit dx mid in
  let finder (_, mit') : bool =
    mtd_comp dx mit mit' = 0
  in
  let sid = get_superclass dx cid in
  if sid = no_idx then no_idx else
    try fst (List.find finder (get_mtds dx sid))
    with Not_found -> get_supermethod dx sid mid

(* get_the_mtd_abstr : dex -> link -> (link -> method_id_item -> bool)
  -> link * method_id_item *)
let rec get_the_mtd_abstr dx (cid: link) finder : link * method_id_item =
  try
    let mid = DA.index_of (finder cid) dx.d_method_ids in
    to_idx mid, get_mit dx (to_idx mid)
  with Not_found ->
  (
    let sid = get_superclass dx cid in
    if sid <> no_idx then get_the_mtd_abstr dx sid finder
    else raise (Wrong_dex ("get_the_mtd: can't find the method"))
  )

(* get_the_mtd : dex -> link -> string -> link * method_id_item *)
let get_the_mtd dx (cid: link) (mname: string) : link * method_id_item =
  let finder (cid': link) (mit: method_id_item) : bool =
    0 = ty_comp dx mit.m_class_id cid' &&
    0 = String.compare mname (get_str dx mit.m_name_id)
  in
  try
    get_the_mtd_abstr dx cid finder
  with (Wrong_dex _) ->
    raise (Wrong_dex ("get_the_mtd: can't find method: "^mname))

(* get_the_mtd_shorty : dex -> link -> string -> string
  -> link * method_id_item *)
let get_the_mtd_shorty dx cid mname shorty : link * method_id_item =
  let finder (cid': link) (mit: method_id_item) : bool =
    let pit = get_pit dx mit in
    0 = String.compare shorty (get_str dx pit.shorty) &&
    0 = ty_comp dx mit.m_class_id cid' &&
    0 = String.compare mname (get_str dx mit.m_name_id)
  in
  get_the_mtd_abstr dx cid finder

(* own_the_mtd : dex -> link -> link -> bool *)
let own_the_mtd dx (cid: link) (mid: link) : bool =
  get_cid_from_mid dx mid = cid

(* get_cdata : dex -> link -> link * class_data_item *)
let get_cdata dx (cid: link) : link * class_data_item =
  let off = (get_cdef dx cid).class_data in
  match get_data_item dx off with
  | CLASS_DATA cdat -> off, cdat
  | _ -> raise (Wrong_match "get_cdata: not CLASS_DATA")

(* get_stt_flds : dex -> link -> (link * encoded_value option) list *)
let get_stt_flds dx (cid: link) : (link * encoded_value option) list =
  let cdef = get_cdef dx cid in
  let _, cdat = get_cdata dx cid in
  let evs =
    if no_off = cdef.static_values then [] else
    match get_data_item dx cdef.static_values with
    | STATIC_VALUE evs -> evs
    | _ -> raise (Wrong_match "get_stt_flds: not STATIC_VALUE")
  in
  let rec iter fids evs =
    match fids, evs with
    | fid::tl1, ev::tl2 -> (fid, Some ev) :: (iter tl1 tl2)
    | fid::tl1, [] -> (fid, None) :: (iter tl1 [])
    | [], [] -> []
  in
  iter (List.map (fun efld -> efld.field_idx) cdat.static_fields) evs

(* get_emtd : dex -> link -> link -> encoded_method *)
let get_emtd dx (cid: link) (mid: link) : encoded_method =
  try
    let emth_finder (emth: encoded_method) : bool =
      mid = emth.method_idx
    and _, cdat = get_cdata dx cid in
    let mtds = cdat.direct_methods @ cdat.virtual_methods in
    List.find emth_finder mtds
  with Not_found ->
    let mname = get_mtd_name dx mid in
    raise (Wrong_dex ("get_emtd: not defined: "^mname))

(* get_citm : dex -> link -> link -> link * code_item *)
let get_citm dx (cid: link) (mid: link) : link * code_item =
  let emth = get_emtd dx cid mid in
  let off = emth.code_off in
  match get_data_item dx off with
  | CODE_ITEM citm -> off, citm
  | _ -> raise (Wrong_match "get_citm: not CODE_ITEM")

(* calc_this : code_item -> int *)
let calc_this (citm: code_item) : int =
  citm.registers_size - citm.ins_size

(* is_param : code_item -> int -> bool *)
let is_param (citm: code_item) (r: int) : bool =
  let this = calc_this citm in
  this <= r && r < citm.registers_size

(***********************************************************************)
(* DEX modification helper                                             *)
(***********************************************************************)

(* empty_section : unit -> section *)
let empty_section () : section =
  {
    size   = 0;
    offset = no_off;
  }

(* empty_dex : unit -> dex *)
let empty_dex () : dex =
  let hd = {
    magic        = "dex\n035";
    checksum     = Int64.zero;
    signature    = [];
    file_size    = 0;
    header_size  = 0x70;
    endian_tag   = LITTLE;
    link         = empty_section ();
    map_off      = no_off;
    h_string_ids = empty_section ();
    h_type_ids   = empty_section ();
    h_proto_ids  = empty_section ();
    h_field_ids  = empty_section ();
    h_method_ids = empty_section ();
    h_class_defs = empty_section ();
    h_data       = empty_section ();
  } in
  {
    header       = hd;
    d_string_ids = DA.create ();
    d_type_ids   = DA.create ();
    d_proto_ids  = DA.create ();
    d_field_ids  = DA.create ();
    d_method_ids = DA.create ();
    d_class_defs = DA.create ();
    d_data       = IM.empty;
  }

(* empty_citm : unit -> code_item *)
let empty_citm () : code_item =
  {
    registers_size = 0;
    ins_size       = 0;
    outs_size      = 0;
    tries_size     = 0;
    debug_info_off = no_off;
    insns_size     = 0;
    insns          = DA.create ();
    tries          = [];
    c_handlers     = [];
  }

(* insrt_data : dex -> link -> data_item -> unit *)
let insrt_data dx (off: link) (data: data_item) : unit =
  dx.d_data <- IM.add (get_off off) data dx.d_data

(* rm_data : dex -> link -> unit *)
let rm_data dx (off: link) : unit =
  dx.d_data <- IM.remove (get_off off) dx.d_data

(* insrt_ins : dex -> link -> I.instr -> unit *)
let insrt_ins dx (off: link) (ins: I.instr) : unit =
  insrt_data dx off (INSTRUCTION ins)

(* insrt_str : dex -> link -> string -> unit *)
let insrt_str dx (off: link) (str: string) : unit =
  insrt_data dx off (STRING_DATA str)

(* insrt_ty_lst : dex -> link -> link list -> unit *)
let insrt_ty_lst dx (off: link) (tl: link list) : unit =
  insrt_data dx off (TYPE_LIST tl)

(* insrt_stt : dex -> link -> encoded_value list -> unit *)
let insrt_stt dx (off: link) (evl : encoded_value list) : unit =
  insrt_data dx off (STATIC_VALUE evl)

(* insrt_citm : dex -> link -> code_item -> unit *)
let insrt_citm dx (off: link) (citm: code_item) : unit =
  insrt_data dx off (CODE_ITEM citm)

