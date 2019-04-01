open Bap_types.Std
open Bap_image_std
open Core_kernel.Std
open Or_error

module Linear = Bap_disasm_linear_sweep
exception Inconsistent_img of string

let read arch ic : (string * addr * addr) list =
  let sym_of_sexp x = [%of_sexp:string * int64 * int64] x in
  let addr_of_int64 x =
    let width = Arch.addr_size arch |> Size.in_bits in
    Addr.of_int64 ~width x in
  List.(Sexp.input_sexps ic >>| sym_of_sexp >>| (fun (s, es, ef) ->
      s, addr_of_int64 es, addr_of_int64 ef))

let read_addrs ic : addr list =
  List.t_of_sexp Addr.t_of_sexp @@ Sexp.input_sexp ic

let ground_truth_of_unstripped_bin bin : addr seq Or_error.t =
  let tmp = Filename.temp_file "bw_" ".symbol" in
  let cmd = sprintf "bap-byteweight dump -i symbols %S > %S" 
      bin tmp in
  if Sys.command cmd = 0
  then return (Seq.of_list @@ In_channel.with_file tmp ~f:read_addrs)
  else errorf
      "failed to fetch symbols from unstripped binary, command `%s'
  failed" cmd

let linear_of_ground_truth bin = 
  let entrances = ground_truth_of_unstripped_bin bin |> ok_exn in
  let img = Common.img_of_filename bin in
  let arch = Image.arch img in
  let segments = Image.segments img in
  Seq.map entrances ~f:(fun entrance -> 
      let segment_for_entrance = 
        match Table.find_addr segments entrance with
        | Some (mem, seg) -> mem
        | None -> 
          raise (Inconsistent_img
                   "Image purports addr spans to not obey them") in
      let mem_for_entrance =
        Memory.view ~from:entrance segment_for_entrance |> ok_exn in
      Linear.sweep arch mem_for_entrance |> ok_exn
    )

