open Core_kernel.Std
open Or_error

open Bap.Std
open Dwarf_types


module Buffer = Dwarf_data.Buffer
module Table = Int.Table
module Input = Dwarf_input

(** Attribute values that we currently understand  *)
module Value = struct
  type hi_addr =
    | Abs of addr
    | Rel of int64
  [@@deriving sexp, bin_io, compare]

  type t =
    | Id of string
    | Lo of addr
    | Hi of hi_addr
  [@@deriving sexp, bin_io, compare, variants]
end

type 'a reader = 'a Dwarf_input.reader
type value = Value.t [@@deriving sexp, bin_io, compare]
type scheme = value option reader list
type table = scheme Table.t


(**   *)
module Fn = struct
  module T = struct
    type t = {
      pc_hi: addr sexp_option;
      pc_lo: addr;
    } [@@deriving sexp, bin_io, compare, fields]

    let int_of_int64 off : int Or_error.t =
      Result.of_option
        ~error:Error.(create "hi pc offset is too big"
                        off sexp_of_int64)
        (Int64.to_int off)

    let create ?pc_hi ~pc_lo () =
      let pc_hi = match pc_hi with
        | None -> return None
        | Some (Value.Abs addr) -> return (Some addr)
        | Some (Value.Rel off) -> int_of_int64 off >>| fun off ->
          Some Addr.(pc_lo ++ off) in
      pc_hi >>| fun pc_hi -> {pc_lo; pc_hi}
    let hash = Hashtbl.hash
    let module_name =  "Bap_Dwarf.Std.Fn"
    let version = "0.1"
  end
  include T
  include Identifiable.Make(struct
      include T
      include Sexpable.To_stringable(T)
    end)
end

type fn = Fn.t [@@deriving sexp, bin_io, compare]
type t = (string * fn) Sequence.t

type spec = {
  endian : endian;
  elf_size : Word_size.t;
  dwarf_size : Word_size.t;
}


let fn_of_values vs : (string * fn) Or_error.t =
  let open Value in
  match List.sort ~cmp:Value.compare vs with
  | Id name :: Lo pc_lo :: Hi pc_hi :: _ ->
    Fn.create ~pc_hi ~pc_lo () >>| fun fn -> name,fn
  | Id name :: Lo pc_lo :: _ ->
    Fn.create ~pc_lo () >>| fun fn -> name,fn
  | Id name :: vs -> errorf "'%s' is degenerate" name |> fun err ->
                     tag_arg err "attrs" vs (sexp_of_list sexp_of_value )
  | vs -> errorf "got anonymous function" |> fun err ->
          tag_arg err "attrs" vs (sexp_of_list sexp_of_value)


let is_done str ~pos_ref =
  Input.(pair char char) str ~pos_ref >>| function
  | 0,0 -> true
  | _ -> pos_ref := !pos_ref - 2; false

let read_scheme s str_sec action str ~pos_ref =
  let address = Input.address s.endian s.elf_size in
  let number = Input.offset s.endian s.dwarf_size in
  let name  = Input.map Input.string ~f:Value.id in
  let lo_pc = Input.map address ~f:Value.lo in
  let hi_pc = Input.map address ~f:(fun x -> Value.(hi (Abs x))) in

  let rec emit attr form =
    match attr,form with
    | Attr.Name,  Form.String -> action name
    | Attr.Low_pc,  Form.Addr -> action lo_pc
    | Attr.High_pc, Form.Addr -> action hi_pc
    | Attr.High_pc, Form.Const len ->
      action (Input.map (Input.const len s.endian)
                ~f:(fun x -> Value.(hi (Rel x))))
    | Attr.Name, Form.Strp ->
      let reader str ~pos_ref =
        number str ~pos_ref >>= fun off ->
        let pos_ref = ref (Buffer.pos str_sec + off) in
        name (Buffer.data str_sec) ~pos_ref in
      action reader
    | _ ->
      let yield = Input.map ~f:(fun _ -> None) in
      let open Form in
      match form with
      | Addr  -> yield address
      | Strp | Offset | Sig -> yield number
      | String -> yield Input.string
      | Block len -> yield (Input.block len s.endian)
      | Ref len | Const len -> yield (Input.const len s.endian)
      | Flag_present -> yield (Input.skip ~bytes:0)
      | Expr   -> yield (Input.block Leb128 s.endian)
      | Indirect ->
        let reader str ~pos_ref =
          Input.form str ~pos_ref >>= fun form ->
          let read = emit attr form in
          read str ~pos_ref in
        yield (reader) in

  let rec loop acc =
    is_done str ~pos_ref >>= function
    | true -> return (List.rev acc)
    | false ->
      Input.(pair attr form) str ~pos_ref >>= fun (attr,form) ->
      loop (emit attr form :: acc) in
  loop []

let rec create_table s str_sec str ~pos_ref =
  let abbrs : table = Table.create () in
  let rec fill () =
    Input.code str ~pos_ref >>= function
    | 0 -> return ()
    | code ->
      Input.tag str ~pos_ref >>= fun tag ->
      Input.skip str ~pos_ref ~bytes:1 >>= fun () -> (* children flag *)
      let action =
        let open Tag in match tag with
        | Subprogram | Entry_point | Inlined_subroutine -> Input.take
        | _ -> Input.drop in
      read_scheme s str_sec action str ~pos_ref
      >>= fun scheme -> match Table.add abbrs ~key:code ~data:scheme with
      | `Ok -> fill ()
      | `Duplicate -> errorf "Duplicate entry code: %d" code in
  fill () >>| fun () -> abbrs

let lookup table code =
  let error =
    Error.create "unknown code" code sexp_of_int in
  Result.of_option ~error @@ Table.find table code


let run_scheme scheme str ~pos_ref : value list Or_error.t =
  List.fold scheme ~init:(Ok []) ~f:(fun vs step ->
      vs >>= fun vs -> step str ~pos_ref >>| function
      | Some v -> v :: vs
      | None -> vs)

let read_function cu_end abbrs str ~pos_ref  =
  let open Sequence.Step in
  if pos_ref.contents < cu_end then
    Input.code str ~pos_ref >>= function
    | 0 -> return @@ Skip ()
    | code -> lookup abbrs code >>= fun scheme ->
      run_scheme scheme str ~pos_ref >>| fun vs ->
      match fn_of_values vs with
      | Ok fn -> Yield (fn, ())
      | Error err -> Skip ()
  else return Done

let create data : t Or_error.t =
  let endian = Dwarf_data.endian data in
  Dwarf_data.section data Section.info   >>= fun info_sec ->
  Dwarf_data.section data Section.abbrev >>= fun abbr_sec ->
  Dwarf_data.section data Section.str    >>= fun str_sec ->
  let info_pos = ref (Buffer.pos info_sec) in
  let abbr_pos = ref (Buffer.pos abbr_sec) in
  let info = Buffer.data info_sec in
  let abbr = Buffer.data abbr_sec in
  let from_info read =
    read info ~pos_ref:info_pos in

  let read_header () =
    from_info (Input.unit_size endian) >>= fun (dwarf_size,unit_size) ->
    let cu_end = !info_pos + unit_size in
    from_info Input.version       >>= fun _version ->
    from_info (Input.offset endian dwarf_size) >>= fun offset ->
    from_info Input.address_size  >>= fun elf_size ->
    abbr_pos := !abbr_pos + offset;
    let s =  { endian; dwarf_size; elf_size } in
    create_table s str_sec abbr ~pos_ref:abbr_pos >>= fun abbrs ->
    return (abbrs, cu_end) in

  let rec read_unit () =
    read_header () >>| fun init ->
    Sequence.unfold_step ~init ~f:(fun (abbrs,cu_end) ->
        let open Sequence.Step in
        match read_function cu_end abbrs info ~pos_ref:info_pos with
        | Ok Done ->
          abbr_pos := Buffer.pos abbr_sec;
          if info_pos.contents < String.length info then
            match read_header () with
            | Ok abbrs -> Skip abbrs
            | Error err ->
              eprintf "Failed to move to a next CU: %s" @@
              (Error.to_string_hum err);
              Done
          else Done
        | Ok (Yield (fn,())) -> Yield (fn,(abbrs,cu_end))
        | Ok (Skip ()) -> Skip (abbrs,cu_end)
        | Error err ->
          eprintf
            "Warning: Dwarf parser stopped prematurely: %s\n\
             \t\tSome symbols maybe ommited\n"
            (Error.to_string_hum err);
          Sequence.Step.Done) in
  read_unit ()

let functions = ident
