open Core_kernel.Std
open Bap.Std
open Bap_ida.Std
open Or_error

let with_byteweight bin : addr seq Or_error.t =
  let tmp = Filename.temp_file "bw_" ".output" in
  let cmd = sprintf "bap-byteweight dump -i byteweight %S > %S" bin tmp in
  if Sys.command cmd = 0
  then return (Seq.of_list @@ In_channel.with_file tmp ~f:Symbols.read_addrs)
  else error_string cmd

let make_addr arch x =
  let width = Size.in_bits (Arch.addr_size arch) in
  Addr.of_int64 ~width x

let with_ida ~which_ida bin : addr seq Or_error.t =
  Image.create bin >>| fun (img, _warns) ->
  let addr = make_addr (Image.arch img) in
  Ida.(with_file ~ida:which_ida bin get_symbols) |>
  Seq.of_list |> Seq.map ~f:(fun (_,_,x) -> addr x)
