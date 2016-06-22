open Core_kernel.Std
open Bap.Std
open Bap_ida.Std
open Bap_future.Std
open Or_error

let with_byteweight bin : addr seq Or_error.t future =
  let tmp = Filename.temp_file "bw_" ".output" in
  let cmd = sprintf "bap-byteweight dump -i byteweight %S > %S" bin tmp in
  let value =
    if Sys.command cmd = 0
    then return (Seq.of_list @@ In_channel.with_file
                   tmp ~f:Symbols.read_addrs)
    else error_string cmd in
  Future.return value


let make_addr arch x =
  let width = Size.in_bits (Arch.addr_size arch) in
  Addr.of_int64 ~width x

let with_ida bin : addr seq Or_error.t future =
  let value =
    let open Or_error in
    Image.create bin >>| fun (img, _warns) ->
    let addr = make_addr (Image.arch img) in
    Ida.(with_file bin get_symbols) |>
    Seq.of_list |> Seq.map ~f:(fun (_,_,x) -> addr x) in
  Future.return value
