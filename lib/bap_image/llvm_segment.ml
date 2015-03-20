open Core_kernel.Std

open Bap_types.Std

type t = {
  name : string;
  address: int64;
  offset : int64;
  length : int;
  bitwidth : int;
  is_readable: bool;
  is_writable: bool;
  is_executable: bool;
}

external segments_of_binary: Llvm_binary.t -> t list 
  = "llvm_binary_segments_stub"

let name t = t.name
let address t = t.address
let offset t = t.offset
let length t = t.length
let bitwidth t = t.bitwidth
let is_readable t = t.is_readable
let is_writable t = t.is_writable
let is_executable t = t.is_executable

let permission_flags s =
  let open Or_error in
  let open Image_backend in
  let check pred flag =
    if pred s then Some flag
    else None in
  let ex = check is_executable X in
  let rd = check is_readable R in
  let wr = check is_writable W in
  let perm = List.fold [ex; rd; wr] ~init:None ~f:(fun perm flag ->
    Option.merge perm flag ~f:(fun p1 p2 -> Or (p1,p2))) in
  match perm with
  | None -> errorf "invalid set of flags"
  | Some perm -> return perm

let to_section t =
  let open Or_error in
  let open Image_backend in
  let open Image_common in
  let name = name t in
  let addr = Bitvector.of_int64 ~width:(bitwidth t) (address t) in
  int_of_int64 (offset t) >>= fun off ->    
  permission_flags t >>= fun perm -> 
    let location = Location.Fields.create ~len:(length t) ~addr in
    return {
      Section.name;
      Section.perm;
      Section.off;
      Section.location;
    } 

