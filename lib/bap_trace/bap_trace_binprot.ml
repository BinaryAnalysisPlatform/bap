
open Core_kernel.Std
open Result
open Bin_prot

open Bap_types.Std

module Proto = struct

  let name = "trace.binprot"
  let probe uri = 
    Uri.scheme uri = Some "file" &&
    Filename.check_suffix (Uri.path uri) ".binprot"

  let supports: 'a tag -> bool = fun _ -> true
 
  let read_from_channel chan buf ~pos ~len = 
    let s = String.create len in
    match In_channel.really_input chan ~buf:s ~pos:0 ~len with
    | None -> raise End_of_file
    | Some () ->
      Bigstring.From_string.blito ~src:s ~dst:buf ~dst_pos:pos ()

  let read reader ch = 
    try 
      let read = read_from_channel ch in
      let value = Utils.bin_read_stream ~read reader in
      Some (Ok value)
    with 
    | End_of_file -> None
    | exn -> Some (Error (Error.of_exn exn))
    
  let write writer value chan =
    let buf = Utils.bin_dump ~header:true writer value in 
    Out_channel.output_string chan (Bigstring.to_string buf)

  let read_tool   = read  Bap_trace.bin_reader_tool
  let read_meta   = read  Value.Dict.bin_reader_t 
  let read_event  = read  Bap_trace.bin_reader_event
  let write_tool  = write Bap_trace.bin_writer_tool
  let write_meta  = write Value.Dict.bin_writer_t
  let write_event = write Bap_trace.bin_writer_event 

end

module TraceWriter = struct

  let make_channel path =
    try
      let fd = Unix.(openfile path [O_WRONLY; O_TRUNC; O_CREAT] 0o666) in
      Ok (Unix.out_channel_of_descr fd)
    with Unix.Unix_error (er,_,_) -> Error (`System_error er)

  let write uri t =
    make_channel (Uri.path uri) >>= 
    fun ch ->
    Proto.write_tool (Bap_trace.tool t) ch;
    Proto.write_meta (Bap_trace.meta t) ch;
    Seq.iter ~f:(fun ev -> Proto.write_event ev ch) (Bap_trace.events t);
    Ok (Out_channel.close ch)  

end

module TraceReader = struct

  let make_channel path = 
    try 
      let () = Unix.(access path [R_OK]) in
      Ok (In_channel.create path)
    with Unix.Unix_error (er,_,_) -> Error (`System_error er)

  let next_event ch = match Proto.read_event ch with
    | None -> In_channel.close ch; None
    | Some ev as res -> res

  let make_header_error () = Error.of_string "trace has damaged header"

  let err_of_opt read ch = match read ch with
    | Some res -> res
    | None -> 
      In_channel.close ch; 
      Error (make_header_error ())

  let make_reader ch =
    let next () = next_event ch in
    err_of_opt Proto.read_tool ch >>=
    fun tool -> err_of_opt Proto.read_meta ch >>=
    fun meta -> Ok (Bap_trace.Reader.({tool; meta; next;}))

  let read uri id =
    make_channel (Uri.path uri) >>=
    fun ch -> match make_reader ch with 
    | Ok r as res -> res
    | Error er -> Error (`Protocol_error er)
  
end

let register () =
  let open Bap_trace in
  let proto = register_proto (module Proto : P) in
  register_reader proto TraceReader.read;
  register_writer proto TraceWriter.write
