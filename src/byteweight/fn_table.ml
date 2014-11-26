open Core_kernel.Std
open Bap.Std
open Or_error

type addr = Addr.t with bin_io,sexp,compare

type path = string

type prob = float with bin_io,sexp,compare

module Fn = struct
  type t = {
    name : string option;
    addr : addr;
    weight : prob;
  } with bin_io,fields,sexp,compare

  let create ?name addr weight : t = {name; addr; weight}
end

type fn = Fn.t with bin_io,sexp,compare
type t = fn table

let default_weight_threshold = 0.5

let decide_signature work_on_byte arch =
  let mode = if work_on_byte then "byte" else "dism" in
  let arch = Arch.to_string arch in
  Printf.sprintf "signatures/%s_%s" arch mode

let fsi sections arch weight_threshold (module LocalMode : Mode.S) : t =
  let rec loop byte_list addr mem tab : t = match byte_list with
    | [] -> tab
    | _ :: tl ->
      let update_tab =
        let key = LocalMode.consecutive ~arch:arch byte_list in
        let weight = LocalMode.find key in
        if weight > weight_threshold then
          let fn = Fn.create addr weight in
          let ttab = Memory.view ~from:addr ~words:1 mem >>= fun m ->
            Table.add tab m fn in
          match ttab with
            | Ok t -> t
            | Error err -> eprintf "New function start adding error: %s\n" @@
            Error.to_string_hum err; tab
        else tab in
      loop tl (Addr.(++) addr 1) mem update_tab in
  Table.foldi sections ~init:Table.empty ~f:(fun mem _ tab ->
    let start = Memory.min_addr mem in
    let byte_list = List.rev (String.to_list_rev (Bigsubstring.to_string
      (Memory.to_buffer mem))) in
    loop byte_list start mem tab
  )

let create ?signatures ?(weight_threshold=default_weight_threshold)
    ?(work_on_bytes=false)
    arch
    ~sections
  : t =
  let sig_file = match signatures with
    | Some s -> s
    | None -> decide_signature work_on_bytes arch in
  let mode =
    Byte.load sig_file;
    (module Byte : Mode.S) in
    (* if work_on_bytes then (
      Byte.load sig_file;
      (module Byte : Mode.S)
    )
    else (
      Dism.load sig_file;
      (module Dism : Mode.S)
    )
    *)
  fsi sections arch weight_threshold mode

let fns tab = Table.elements tab

let addrs tab = Table.elements (Table.map tab ~f:(fun fn -> Fn.addr fn))
