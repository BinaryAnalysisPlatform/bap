(*
 * Copyright (c) 2010-2014,
 *  Jinseong Jeon <jsjeon@cs.umd.edu>
 *  Kris Micinski <micinski@cs.umd.edu>
 *  Jeff Foster   <jfoster@cs.umd.edu>
 * All rights reserved.
 *
 * Based on the src/parse.ml from https://github.com/plum-umd/redexer
 *)

(***********************************************************************)
(* Parse                                                               *)
(* reference: dalvik/libdex/Leb128.c,                                  *)
(*            dalvik/dx/rop/cst/CstUtf8.java                           *)
(***********************************************************************)

module DA = DynArray

module Instr = Dalvik_disasm
module Dex = Dalvik_dex
module Java = Dalvik_java

module IM = Map.Make(Int32)
module IS = Set.Make(Int32)

(* TODO: use BAP logging instead? *)

(***********************************************************************)
(* Utilities/Reading                                                   *)
(***********************************************************************)

let to_i32 x = Int32.of_int x
let to_i64 x = Int64.of_int x

(* reading 8 bit *)
let read8 (chan: in_channel) : int =
  int_of_char (input_char chan)

(* reading 8 bit - signed *)
let read8s (chan: in_channel) : int =
  let x = read8 chan in
  if (x land 0x80) <> 0 then x - 0x100 else x

(* reading 16 bit *)
let read16 (chan: in_channel) : int =
  let l = read8 chan in
  let h = read8 chan in
  (h lsl 8) lor l

(* reading 32 bit *)
let read32 (chan: in_channel) : int =
  let l = read16 chan in
  let h = read16 chan in
  Int32.to_int (to_i32 ((h lsl 16) lor l))

(* reading 32 bit - unsign *)
let read32u (chan: in_channel) : int64 =
  let l = to_i64 (read16 chan) in
  let h = to_i64 (read16 chan) in
  Int64.logor (Int64.shift_left h 16) l

(* reading 64 bit *)
let read64 (chan: in_channel) : int64 =
  let l = read32u chan in
  let h = read32u chan in
  Int64.logor (Int64.shift_left h 32) l

let until (sz: int) : int list =
  let rec gen i j : int list =
    if i = j then [i] else i :: (gen (i+1) j)
  in
  gen 0 sz

(* reading unsigned LEB128 *)
let read_uleb128 (chan: in_channel) : int =
  let extender (res, cur) i =
    if cur > 0x7f then
      let cur' = read8 chan in
      let res' = res lor ((cur' land 0x7f) lsl (i * 7)) in
      (res', cur')
    else (res, cur)
  in
  fst (List.fold_left extender (0, 0x80) (until 4))

(* reading unsigned LEB128 - 1 *)
let read_uleb128p1 (chan: in_channel) : int =
  (read_uleb128 chan) - 1

(* reading signed LEB128 *)
let read_sleb128 (chan: in_channel) : int =
  let extender (res, cur, pos) i =
    if cur > 0x7f then
      let cur' = read8 chan in
      let res' = res lor ((cur' land 0x7f) lsl (i * 7)) in
      (res', cur', i * 7)
    else (res, cur, pos)
  in
  let ret, _, pos = List.fold_left extender (0, 0x80, 0) (until 4) in
  if ((0x1 lsl (pos + 6)) land ret) <> 0 then
    ret - (0x1 lsl (pos + 7))
  else ret

(* reading variable-length number *)
let read_VLN (chan: in_channel) (sz: int) (signed: bool) : int64 =
  let extender (res, last) i =
    let cur = read8 chan in
    Int64.logor res (Int64.shift_left (to_i64 cur) (i * 8)), cur
  in
  let ret, b = List.fold_left extender (Int64.zero, 0) (until (sz - 1)) in
  if signed && (b land 0x80) <> 0 && sz < 8 then
    let fill_0xff res i =
      Int64.logor res (Int64.shift_left (Int64.of_int (0xff)) ((i+sz)*8))
    in
    List.fold_left fill_0xff ret (until (8 - sz - 1))
  else ret

(* reading variable-length floating-point number *)
let read_fVLN (chan: in_channel) (sz: int) : int64 =
  let extender res i =
    let cur = read8 chan in
    Int64.logor res (Int64.shift_left (to_i64 cur) (i * 8))
  in
  let ret = List.fold_left extender Int64.zero (until (sz - 1)) in
  Int64.shift_left ret ((8 - sz) * 8)

(* reading 16 bit index *)
let read_idx16 (chan: in_channel) : Dex.link =
  Dex.to_idx (read16 chan)

(* reading 32 bit index *)
let read_idx32 (chan: in_channel) : Dex.link =
  let i = read32u chan in
  if Int64.compare i (Int64.of_string "0xffffffff") = 0
  then Dex.no_idx
  else Dex.to_idx (Int64.to_int i)

(* reading offset *)
let read_off (chan: in_channel) : Dex.link =
  let i = read32u chan in
  Dex.to_off (Int64.to_int i)

(* byte alignment *)
let alignment (chan: in_channel) (n: int) : int =
  let pos = pos_in chan in
  let r = pos mod n in
  if r <> 0 then List.iter (fun _ -> ignore (read8 chan)) (until (r - 1));
  r

(***********************************************************************)
(* Validation                                                          *)
(***********************************************************************)

(* validating channel position *)
let is_validate (chan: in_channel) (s: Dex.section) (msg: string) : unit =
  let cur = pos_in chan in
  match s.Dex.offset with
  | Dex.Off off ->
    if cur <> (Int32.to_int off) && s.Dex.size > 0 then raise (Dex.Wrong_dex msg)
  | _ -> raise (Dex.Wrong_dex "is_validate")

(* validating type strings *)
let validate_type (dx: Dex.dex) : unit =
  let iter_type (id: Dex.link) : unit =
    let str = Dex.get_str dx id in
    if not (Java.is_type_descr str) then
      Log.w ("not type descryption: "^str)
  and iter_proto (it: Dex.proto_id_item) : unit =
    let str = Dex.get_str dx it.Dex.shorty in
    if not (Java.is_shorty_descr str) then
      Log.w ("not shorty descryption: "^str)
  in
  DA.iter iter_type  dx.Dex.d_type_ids;
  DA.iter iter_proto dx.Dex.d_proto_ids

(***********************************************************************)
(* Parsing                                                             *)
(***********************************************************************)

(* return int type offset if it is non-zero *)
let get_off (offset: Dex.link) : int32 * int option =
  match offset with
  | Dex.Off off ->
    let ioff = Int32.to_int off in
    if ioff = Dex.no_offset then off, None else off, Some ioff
  | _ -> raise (Dex.Wrong_dex "get_off")

(* making list of any item, e.g. all link structures *)
let rec parse_list ch (job: in_channel -> 'a) (length: int) : 'a list =
  if length = 0 then [] else
  let elt = job ch in elt :: (parse_list ch job (length-1))

(* reading encoded value *)
let rec read_encoded_value (chan: in_channel) : Dex.encoded_value =
  let byte  = read8 chan in
  let v_arg = byte lsr 5 and v_ty = byte land 0x1f in
  match v_ty with
  | 0x00 ->
    let v = read8s chan in Dex.VALUE_BYTE (to_i64 v)
  | 0x02 ->
    let v = read_VLN chan (v_arg+1) true  in Dex.VALUE_SHORT v
  | 0x03 ->
    let v = read_VLN chan (v_arg+1) false in Dex.VALUE_CHAR  v
  | 0x04 ->
    let v = read_VLN chan (v_arg+1) true  in Dex.VALUE_INT   v
  | 0x06 ->
    let v = read_VLN chan (v_arg+1) true  in Dex.VALUE_LONG  v
  | 0x10 ->
    let v = read_fVLN chan (v_arg+1) in
    Dex.VALUE_FLOAT (Int64.shift_right_logical v 32)
  | 0x11 ->
    let v = read_fVLN chan (v_arg+1) in Dex.VALUE_DOUBLE v
  | 0x17 | 0x18 | 0x19 | 0x1a | 0x1b ->
  (
    let id = Int64.to_int (read_VLN chan (v_arg+1) false) in
    match v_ty with
    | 0x17 -> Dex.VALUE_STRING id
    | 0x18 -> Dex.VALUE_TYPE   id
    | 0x19 -> Dex.VALUE_FIELD  id
    | 0x1a -> Dex.VALUE_METHOD id
    | 0x1b -> Dex.VALUE_ENUM   id
    | _ -> raise (Dex.Wrong_match "read_encoded_value: 0x17 ~ 0x1b")
  )
  | 0x1c ->
    let sz = read_uleb128 chan in
    Dex.VALUE_ARRAY (parse_list chan read_encoded_value sz)
  | 0x1d -> Dex.VALUE_ANNOTATION (parse_encoded_ann chan)
  | 0x1e -> Dex.VALUE_NULL
  | 0x1f -> Dex.VALUE_BOOLEAN (v_arg > 0)
  | _ -> raise (Dex.Wrong_dex ("illegal ev type :"^(string_of_int v_ty)))

and parse_encoded_ann (ch: in_channel) : Dex.encoded_annotation =
  let id = read_uleb128 ch
  and sz = read_uleb128 ch in
  let parse_elts ch' =
    let nm = read_uleb128 ch'
    and  v = read_encoded_value ch' in
    {
      Dex.name_idx = Dex.to_idx nm;
      Dex.value    = v;
    }
  in
  let elts = parse_list ch parse_elts sz in
  {
    Dex.an_type_idx = Dex.to_idx id;
    Dex.elements    = elts;
  }

(* from length of string to unicode encoded string *)
let build_string ch len : UTF8.t =
  let buf = UTF8.Buf.create len in
  let add_buf i =
    UTF8.Buf.add_char buf (UChar.uchar_of_int i)
  in
  for i = 0 to (len - 1) do
    let v0 = read8 ch in
    let v4 = v0 lsr 4 in
    if 0x00 <= v4 && v4 <= 0x07 then (* single-byte encoding *)
      add_buf v0
    else if 0x0c <= v4 && v4 <= 0x0d then (* two-byte encoding *)
    (
      let v1 = read8 ch in
      if v1 land 0xc0 <> 0x80 then raise (Dex.Wrong_dex "bad MUTF-8, 2bytes-v1");
      let v = ((v0 land 0x1f) lsl 6) lor (v1 land 0x3f) in
      if v <> 0 && v < 0x80 then raise (Dex.Wrong_dex "bad MUTF-8, 2bytes-v");
      add_buf v
    )
    else if v4 = 0x0e then (* three-byte encoding *)
    (
      let v1 = read8 ch in
      if v1 land 0xc0 <> 0x80 then raise (Dex.Wrong_dex "bad MUTF-8, 3bytes-v1");
      let v2 = read8 ch in
      if v2 land 0xc0 <> 0x80 then raise (Dex.Wrong_dex "bad MUTF-8, 3bytes-v2");
      let v = ((v0 land 0x0f) lsl 12) lor
        (((v1 land 0x3f) lsl 6) lor (v2 land 0x3f)) in
      if v < 0x800 then raise (Dex.Wrong_dex "bad MUTF-8, 3bytes-v");
      add_buf v
    )
    else raise (Dex.Wrong_dex "bad MUTF-8, only 1-3 bytes")
  done;
  UTF8.Buf.contents buf

(* parse : in_channel -> Dex.dex *)
let rec parse (ch: in_channel) : Dex.dex =
  let header = parse_header ch
  in
  is_validate ch header.Dex.h_string_ids "string_ids";
  let string_ids = parse_list ch read_off header.Dex.h_string_ids.Dex.size
  in
  is_validate ch header.Dex.h_type_ids "type_ids";
  let type_ids   = parse_list ch read_idx32 header.Dex.h_type_ids.Dex.size
  in
  is_validate ch header.Dex.h_proto_ids "proto_ids";
  let proto_ids  =
    let get_proto = fun ch ->
      let sh = read_idx32 ch
      and rt = read_idx32 ch
      and pa = read_off ch in
      { Dex.shorty = sh; Dex.return_type = rt; Dex.parameter_off = pa; } in
    parse_list ch get_proto header.Dex.h_proto_ids.Dex.size
  in
  is_validate ch header.Dex.h_field_ids "field_ids";
  let field_ids  =
    let get_field = fun ch ->
      let cid = read_idx16 ch
      and fid = read_idx16 ch
      and nid = read_idx32 ch in
      { Dex.f_class_id = cid; Dex.f_type_id = fid; Dex.f_name_id = nid; } in
    parse_list ch get_field header.Dex.h_field_ids.Dex.size
  in
  is_validate ch header.Dex.h_method_ids "method_ids";
  let method_ids =
    let get_method = fun ch ->
      let cid = read_idx16 ch
      and pid = read_idx16 ch
      and nid = read_idx32 ch in
      { Dex.m_class_id = cid; Dex.m_proto_id = pid; Dex.m_name_id = nid; } in
    parse_list ch get_method header.Dex.h_method_ids.Dex.size
  in
  is_validate ch header.Dex.h_class_defs "class_defs";
  let class_defs =
    let get_class = fun ch ->
      let cid = read_idx32 ch
      and acs = read32 ch
      and sup = read_idx32 ch
      and ifo = read_off ch
      and src = read_idx32 ch
      and ann = read_off ch
      and cdo = read_off ch
      and stt = read_off ch in
      {
        Dex.c_class_id    = cid;
        Dex.c_access_flag = acs;
        Dex.superclass    = sup;
        Dex.interfaces    = ifo;
        Dex.source_file   = src;
        Dex.annotations   = ann;
        Dex.class_data    = cdo;
        Dex.static_values = stt;
      } in
    parse_list ch get_class header.Dex.h_class_defs.Dex.size
  in
  let data = parse_data ch string_ids proto_ids class_defs in
  let  map = parse_map  ch header.Dex.map_off data in
  let dex = {
    Dex.header       = header;
    Dex.d_string_ids = DA.of_list string_ids;
    Dex.d_type_ids   = DA.of_list type_ids;
    Dex.d_proto_ids  = DA.of_list proto_ids;
    Dex.d_field_ids  = DA.of_list field_ids;
    Dex.d_method_ids = DA.of_list method_ids;
    Dex.d_class_defs = DA.of_list class_defs;
    Dex.d_data       = map;
  } in
  validate_type dex;
  dex

(* parsing header *)
and parse_header (ch: in_channel) : Dex.dex_header =
  let mg = build_string ch 8
  and ck = read32u ch
  and si = parse_list ch (fun ch' -> char_of_int (read8 ch')) 20
  and fs = read32 ch
  and hs = read32 ch
  and ed = match read32 ch with
    | 0x12345678 -> Dex.LITTLE
    | 0x78563412 -> Dex.BIG
    | _ -> raise (Dex.Wrong_dex "endian_tag")
  and lk = parse_section ch
  and mo = read_off ch
  and st = parse_section ch
  and ty = parse_section ch
  and pt = parse_section ch
  and fd = parse_section ch
  and md = parse_section ch
  and cl = parse_section ch
  and dt = parse_section ch
  in
  {
    Dex.magic        = mg;
    Dex.checksum     = ck;
    Dex.signature    = si;
    Dex.file_size    = fs;
    Dex.header_size  = hs;
    Dex.endian_tag   = ed;
    Dex.link         = lk;
    Dex.map_off      = mo;
    Dex.h_string_ids = st;
    Dex.h_type_ids   = ty;
    Dex.h_proto_ids  = pt;
    Dex.h_field_ids  = fd;
    Dex.h_method_ids = md;
    Dex.h_class_defs = cl;
    Dex.h_data       = dt;
  }

and parse_section (ch: in_channel) : Dex.section =
  let size = read32 ch
  and off  = read_off ch in
  { Dex.size = size; Dex.offset = off; }

(* parsing data pool *)
and parse_data ch string_ids proto_ids class_defs : Dex.data_item IM.t =
  let str_map = parse_string ch string_ids IM.empty in
  let prt_map = parse_proto  ch  proto_ids  str_map in
  let cls_map = parse_class  ch class_defs  prt_map in cls_map

and parse_string ch string_ids map : Dex.data_item IM.t =
  let parse_str_item acc l : Dex.data_item IM.t =
    match get_off l with
    | off, Some ioff ->
    (
      seek_in ch ioff;
      let sz = read_uleb128 ch in
      let str = build_string ch sz in
      ignore (read8 ch); (* null termination *)
      IM.add off (Dex.STRING_DATA str) acc
    )
    | _, _ -> acc
  in
  List.fold_left parse_str_item map string_ids

and parse_type_list ch offset map : Dex.data_item IM.t =
  match get_off offset with
  | off, Some ioff ->
  (
    seek_in ch ioff;
    let sz = read32 ch in
    let type_idx = parse_list ch read_idx16 sz in
    ignore (alignment ch 4);
    IM.add off (Dex.TYPE_LIST type_idx) map
  )
  | _, _ -> map

and parse_proto ch proto_ids map : Dex.data_item IM.t =
  let each_proto acc (p: Dex.proto_id_item) : Dex.data_item IM.t =
    parse_type_list ch p.Dex.parameter_off acc
  in
  List.fold_left each_proto map proto_ids

(* parsing class *)
and parse_class ch class_defs map : Dex.data_item IM.t =
  let each_cdefs acc (def: Dex.class_def_item) : Dex.data_item IM.t =
    let itf_map = parse_type_list   ch def.Dex.interfaces    acc     in
    let ann_map = parse_annotations ch def.Dex.annotations   itf_map in
    let cld_map = parse_class_data  ch def.Dex.class_data    ann_map in
    let stt_map = parse_static_val  ch def.Dex.static_values cld_map in
    stt_map
  in
  List.fold_left each_cdefs map class_defs

and parse_annotations ch offset map : Dex.data_item IM.t =
  match get_off offset with
  | off, Some ioff ->
  (
    seek_in ch ioff;
    let cls = read_off ch
    and fsz = read32 ch
    and msz = read32 ch
    and psz = read32 ch
    and parse_anno_off ch' : Dex.anno_off =
      let idx = read_idx32 ch'
      and aff = read_off ch' in
      {
        Dex.target         = idx;
        Dex.annotation_off = aff;
      }
    in
    let fds = parse_list ch parse_anno_off fsz
    and mds = parse_list ch parse_anno_off msz
    and pam = parse_list ch parse_anno_off psz in
    let ann =
    {
      Dex.class_anno_off = cls;
      Dex.fields         = fds;
      Dex.methods        = mds;
      Dex.parameters     = pam;
    } in
    let ann_map = IM.add off (Dex.ANNO_DIR ann) map in
    let cls_map = parse_anno_set ch cls ann_map in
    let f_set = fun acc f -> parse_anno_set ch f.Dex.annotation_off acc
    and f_ref = fun acc f -> parse_anno_ref ch f.Dex.annotation_off acc in
    let fds_map = List.fold_left f_set cls_map fds in
    let mds_map = List.fold_left f_set fds_map mds in
                  List.fold_left f_ref mds_map pam
  )
  | _, _ -> map

and parse_anno_ref ch offset map : Dex.data_item IM.t =
  match get_off offset with
  | off, Some ioff ->
  (
    seek_in ch ioff;
    let sz  = read32 ch in
    let lst = parse_list ch read_off sz in
    let ref_map = IM.add off (Dex.ANNO_SET_REF lst) map in
    List.fold_left (fun acc l -> parse_anno_set ch l acc) ref_map lst
  )
  | _, _ -> map

and parse_anno_set ch offset map : Dex.data_item IM.t =
  match get_off offset with
  | off, Some ioff ->
  (
    seek_in ch ioff;
    let sz  = read32 ch in
    let lst = parse_list ch read_off sz in
    let set_map = IM.add off (Dex.ANNO_SET lst) map in
    List.fold_left (fun acc l -> parse_anno_item ch l acc) set_map lst
  )
  | _, _ -> map

and parse_anno_item ch offset map : Dex.data_item IM.t =
  match get_off offset with
  | off, Some ioff ->
  (
    seek_in ch ioff;
    let ub  = read8 ch in
    let vis = match ub with
      | 0x00 -> Dex.VISIBILITY_BUILD
      | 0x01 -> Dex.VISIBILITY_RUNTIME
      | 0x02 -> Dex.VISIBILITY_SYSTEM
      |    _ -> raise (Dex.Wrong_dex ("visibility: "^(string_of_int ub)))
    in
    let ann = parse_encoded_ann ch in
    let elt =
    {
      Dex.visible    = vis;
      Dex.annotation = ann;
    }
    in IM.add off (Dex.ANNOTATION elt) map
  )
  | _, _ -> map

and parse_class_data ch offset map : Dex.data_item IM.t =
  match get_off offset with
  | off, Some ioff ->
  (
    seek_in ch ioff;
    let s_fd_sz = read_uleb128 ch
    and i_fd_sz = read_uleb128 ch
    and d_md_sz = read_uleb128 ch
    and v_md_sz = read_uleb128 ch in
    let prev = ref 0 in
    let parse_encoded_field  ch' : Dex.encoded_field  =
      let id_dif = read_uleb128 ch'
      and ac_flg = read_uleb128 ch' in
      prev := (!prev + id_dif);
      {
        Dex.field_idx     = Dex.to_idx !prev;
        Dex.f_access_flag = ac_flg;
      }
    and parse_encoded_method ch' : Dex.encoded_method =
      let id_dif = read_uleb128 ch'
      and ac_flg = read_uleb128 ch'
      and cd_off = Dex.to_off (read_uleb128 ch') in
      prev := (!prev + id_dif);
      {
        Dex.method_idx    = Dex.to_idx !prev;
        Dex.m_access_flag = ac_flg;
        Dex.code_off      = cd_off;
      } in
    prev := 0;
    let s_fd = parse_list ch parse_encoded_field  s_fd_sz in
    prev := 0;
    let i_fd = parse_list ch parse_encoded_field  i_fd_sz in
    prev := 0;
    let d_md = parse_list ch parse_encoded_method d_md_sz in
    prev := 0;
    let v_md = parse_list ch parse_encoded_method v_md_sz in
    let c_data =
    {
      Dex.static_fields   = s_fd;
      Dex.instance_fields = i_fd;
      Dex.direct_methods  = d_md;
      Dex.virtual_methods = v_md;
    } in
    let cmap = IM.add off (Dex.CLASS_DATA c_data) map in
    let md_folder acc (emd: Dex.encoded_method) : Dex.data_item IM.t =
      parse_code ch emd.Dex.code_off acc
    in
    let map' = List.fold_left md_folder cmap d_md in
               List.fold_left md_folder map' v_md
  )
  | _, _ -> map

and parse_code ch offset map : Dex.data_item IM.t =
  match get_off offset with
  | off, Some ioff ->
  (
    seek_in ch ioff;
    let reg_sz = read16 ch
    and ins_sz = read16 ch
    and out_sz = read16 ch
    and tri_sz = read16 ch
    and d_info = read_off ch
    and ist_sz = 2 * (read32 ch) in
    let prv_ins = pos_in ch
    and insns, ins_map = parse_instr ch ist_sz map in
    seek_in ch (prv_ins + ist_sz);
    let tries, handles =
    if tri_sz <> 0 then
    (
      if (ist_sz / 2) mod 2 <> 0 then ignore (read16 ch); (* padding *)
      let base = Dex.of_off (DA.get insns 0) in
      let tri = parse_list ch (parse_try ins_map base) tri_sz
      and hdl_sz = read_uleb128 ch in
      let hdl = parse_list ch (parse_handle base) hdl_sz in
      convert_handler_off tri, hdl
    ) else [], [] in
    let c_item =
    {
      Dex.registers_size = reg_sz;
      Dex.ins_size       = ins_sz;
      Dex.outs_size      = out_sz;
      Dex.tries_size     = tri_sz;
      Dex.debug_info_off = d_info;
      Dex.insns_size     = ist_sz;
      Dex.insns          = insns;
      Dex.tries          = tries;
      Dex.c_handlers     = handles;
    } in
    ignore (alignment ch 4);
    let code_map = IM.add off (Dex.CODE_ITEM c_item) ins_map in
    parse_debug_info ch d_info code_map
  )
  | _, _ -> map

and parse_try map base ch : Dex.try_item =
  let sta = base + 2 * (Int64.to_int (read32u ch))
  and cnt = 2 * (read16 ch)
  and off = read16 ch in
  let find_valid_addr (found, addr) offset =
    if found then (found, addr) else
    try
      let addr' = sta + cnt - offset in
      ignore (IM.find (Int32.of_int addr') map);
      (true, addr')
    with Not_found -> (false, addr)
  in
  let _, edr = List.fold_left find_valid_addr (false, sta + cnt) [2;4;6;10] in
  {
    Dex.start_addr  = Dex.to_off sta;
    Dex.end_addr    = Dex.to_off edr;
    Dex.handler_off = Dex.to_off off;
  }

and convert_handler_off tries : Dex.try_item list =
  let collect_off acc (ti: Dex.try_item) =
    IS.add (Dex.get_off ti.Dex.handler_off) acc
  in
  let offs = List.fold_left collect_off IS.empty tries in
  let hash = Hashtbl.create 9 in
  let make_hash i off = Hashtbl.add hash (Dex.Off off) i in
  DA.iteri make_hash (DA.of_list (IS.elements offs));
  let convert_off ti : Dex.try_item = { ti with
    Dex.handler_off = Dex.to_idx (Hashtbl.find hash ti.Dex.handler_off)
  } in
  List.map convert_off tries

and parse_handle base ch : Dex.encoded_catch_handler =
  let parse_ty_addr ch' =
    let id = read_uleb128 ch'
    and ad = base + 2 * (read_uleb128 ch') in
    {
      Dex.ch_type_idx = Dex.to_idx id;
      Dex.addr        = Dex.to_off ad;
    }
  and sz = read_sleb128 ch in
  let handlers = parse_list ch parse_ty_addr (abs sz) in
  let ch_all = if sz <= 0 then base + 2 * (read_uleb128 ch) else 0 in
  {
    Dex.e_handlers     = handlers;
    Dex.catch_all_addr = Dex.to_off ch_all;
  }

and parse_instr ch sz map : Dex.link DA.t * Dex.data_item IM.t =
  let off_lst = ref ([]: int list)
  and itm_lst = ref ([]: Dex.data_item list)
  and max_off = ref ((pos_in ch) + sz)
  in
  while pos_in ch <= (!max_off - 2) do
    let off = pos_in ch in
    let hx = read8 ch in
    let op, ins_sz = Instr.hx_to_op_and_size hx in
    if 0 = ins_sz then
      raise (Dex.Wrong_dex ("undefined op: "^(string_of_int hx)))
    else
    (
      let args = parse_list ch read8 (ins_sz - 1) in
      let p_ins = Instr.make_instr op args in
      let itm = Dex.INSTRUCTION
      (
        let rec to_abs_addr base = function
          | [] -> []
          | [Instr.OPR_OFFSET i32] ->
            let abs = base + ((Int32.to_int i32) * 2)
            in [Instr.OPR_OFFSET (to_i32 abs)]
          | hd::tl -> hd::(to_abs_addr base tl)
        in
        match op with
        | Instr.OP_GOTO   | Instr.OP_GOTO_16 | Instr.OP_GOTO_32
        | Instr.OP_IF_EQ  | Instr.OP_IF_NE   | Instr.OP_IF_LT
        | Instr.OP_IF_GE  | Instr.OP_IF_GT   | Instr.OP_IF_LE
        | Instr.OP_IF_EQZ | Instr.OP_IF_NEZ  | Instr.OP_IF_LTZ
        | Instr.OP_IF_GEZ | Instr.OP_IF_GTZ  | Instr.OP_IF_LEZ ->
          op, to_abs_addr off (snd p_ins)
        | Instr.OP_FILL_ARRAY_DATA ->
        (
          let cur_off = pos_in ch in
          let n_args = to_abs_addr off (snd p_ins) in
          (
            match n_args with
            | _::Instr.OPR_OFFSET i32::[] ->
            (
              let off' = Int32.to_int i32 in
              seek_in ch off';
              let ident = read16 ch in
              if ident <> 0x0300 then
                raise (Dex.Wrong_dex ("fill-array-data: ident"))
              else
              (
                let ar_wd = read16 ch
                and ar_sz = read32 ch in
                let reader ch = match ar_wd with
                  | 1 -> Instr.OPR_CONST (to_i64 (read8  ch))
                  | 2 -> Instr.OPR_CONST (to_i64 (read16 ch))
                  | 4 -> Instr.OPR_CONST (read32u ch)
                  | 8 -> Instr.OPR_CONST (read64  ch)
                  | _ -> raise (Dex.NOT_YET (string_of_int ar_wd))
                in
                let ar_data = parse_list ch reader ar_sz in
                let item = Dex.FILL_ARRAY
                {
                  Dex.ad_width = ar_wd;
                  Dex.ad_size  = ar_sz;
                  Dex.ad_data  = ar_data;
                } in
                off_lst := off' :: !off_lst;
                itm_lst := item :: !itm_lst;
                max_off := min !max_off off'
              )
            )
            | _ -> raise (Dex.Wrong_dex ("fill-array-data: args"))
          );
          seek_in ch cur_off;
          op, n_args
        )
        | Instr.OP_PACKED_SWITCH
        | Instr.OP_SPARSE_SWITCH ->
        (
          let cur_off = pos_in ch in
          let n_args = to_abs_addr off (snd p_ins) in
          (
            match n_args with
            | _::Instr.OPR_OFFSET i32::[] ->
            (
              let off' = Int32.to_int i32 in
              seek_in ch off';
              let ident = read16 ch in
              if ident <> 0x0100 && ident <> 0x0200 then
                raise (Dex.Wrong_dex ("switch: ident"))
              else
              (
                let sw_sz = read16 ch in
                let sw_k =
                  if 0x0100 = ident then [read32 ch]
                  else parse_list ch read32 sw_sz
                in
                let make_off ch =
                  let offset = read32 ch in
                  let addr = off + (offset * 2) in Dex.to_off addr
                in
                let sw_tg = parse_list ch make_off sw_sz in
                let item = Dex.SWITCH
                {
                  Dex.sw_base    = Dex.to_off off;
                  Dex.sw_size    = sw_sz;
                  Dex.sw_keys    = sw_k;
                  Dex.sw_targets = sw_tg;
                } in
                off_lst := off' :: !off_lst;
                itm_lst := item :: !itm_lst;
                max_off := min !max_off off'
              )
            )
            | _ -> raise (Dex.Wrong_dex ("switch: args"))
          );
          seek_in ch cur_off;
          op, n_args
        )
        | _ -> p_ins
      )
      in
      off_lst := off :: !off_lst;
      itm_lst := itm :: !itm_lst
    )
  done;
  let inserter acc off itm : Dex.data_item IM.t =
    IM.add (Int32.of_int off) itm acc
  and f off1 off2 = Pervasives.compare off1 off2
  in
  DA.of_list (List.map Dex.to_off (L.stable_sort f !off_lst)),
  List.fold_left2 inserter map !off_lst !itm_lst

and parse_debug_info ch offset map : Dex.data_item IM.t =
  match get_off offset with
  | off, Some ioff ->
  (
    seek_in ch ioff;
    let line = read_uleb128 ch
    and p_sz = read_uleb128 ch
    and parse_name ch' = Dex.to_idx (read_uleb128p1 ch') in
    let parm = parse_list ch parse_name p_sz
    and prv  = ref 1
    and dbgm = ref ([]: (Dex.state_machine_instr * Instr.operand list) list) in
    while !prv <> 0x00 do
      prv := read8 ch;
      match !prv with
      | 0x00 ->
        dbgm := (Dex.DBG_END_SEQUENCE, []) :: !dbgm
      | 0x01 ->
        let addr_diff = to_i64 (read_uleb128 ch) in
        dbgm := (Dex.DBG_ADVANCE_PC, [Instr.OPR_CONST addr_diff]) :: !dbgm
      | 0x02 ->
        let line_diff = to_i64 (read_sleb128 ch) in
        dbgm := (Dex.DBG_ADVANCE_LINE, [Instr.OPR_CONST line_diff]) :: !dbgm
      | 0x03 ->
        let v = Instr.OPR_REGISTER (read_uleb128 ch)
        and n = Instr.OPR_INDEX  (read_uleb128p1 ch)
        and t = Instr.OPR_INDEX  (read_uleb128p1 ch) in
        dbgm := (Dex.DBG_START_LOCAL, [v;n;t]) :: !dbgm
      | 0x04 ->
        let v = Instr.OPR_REGISTER (read_uleb128 ch)
        and n = Instr.OPR_INDEX  (read_uleb128p1 ch)
        and t = Instr.OPR_INDEX  (read_uleb128p1 ch)
        and s = Instr.OPR_INDEX  (read_uleb128p1 ch) in
        dbgm := (Dex.DBG_START_LOCAL_EXTENDED, [v;n;t;s]) :: !dbgm
      | 0x05 ->
        let v = Instr.OPR_REGISTER (read_uleb128 ch) in
        dbgm := (Dex.DBG_END_LOCAL, [v]) :: !dbgm
      | 0x06 ->
        let v = Instr.OPR_REGISTER (read_uleb128 ch) in
        dbgm := (Dex.DBG_RESTART_LOCAL, [v]) :: !dbgm
      | 0x07 ->
        dbgm := (Dex.DBG_SET_PROLOGUE_END, []) :: !dbgm
      | 0x08 ->
        dbgm := (Dex.DBG_SET_EPILOGUE_BEGIN, []) :: !dbgm
      | 0x09 ->
        let n = Instr.OPR_INDEX (read_uleb128p1 ch) in
        dbgm := (Dex.DBG_SET_FILE, [n]) :: !dbgm
      |    _ ->
        let op = Instr.OPR_CONST (to_i64 !prv) in
        dbgm := (Dex.DBG_SPECIAL, [op]) :: !dbgm
    done;
    let d_info =
    {
      Dex.line_start     = line;
      Dex.parameter_name = parm;
      Dex.state_machine  = List.rev !dbgm;
    } in
    IM.add off (Dex.DEBUG_INFO d_info) map
  )
  | _, _ -> map

and parse_static_val ch offset map : Dex.data_item IM.t =
  match get_off offset with
  | off, Some ioff ->
  (
    seek_in ch ioff;
    let size = read_uleb128 ch in
    let values = parse_list ch read_encoded_value size in
    IM.add off (Dex.STATIC_VALUE values) map
  )
  | _, _ -> map

and parse_map ch offset map : Dex.data_item IM.t =
  match get_off offset with
  | off, Some ioff ->
  (
    let parse_map_item ch' : Dex.map_item =
      let ty = read16 ch' in
      ignore (read16 ch');
      let sz = Int64.to_int (read32u ch')
      and os = read_off ch' in
      {
        Dex.type_of_item = Dex.to_type_code ty;
        Dex.mi_size      = sz;
        Dex.mi_offset    = os;
      }
    in
    seek_in ch ioff;
    let size = Int64.to_int (read32u ch) in
    let mlst = parse_list ch parse_map_item size in
    IM.add off (Dex.MAP_LIST mlst) map
  )
  | _, _ -> map

