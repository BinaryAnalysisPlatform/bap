open Core_kernel.Std
open Or_error
open Bap.Std

type response = Ezjsonm.t
type request = Ezjsonm.t
type uri = Uri.t

type severity =
  | Critical
  | Error
  | Warning
with bin_io, compare, sexp

let string_of_severity s =
  Sexp.to_string @@ sexp_of_severity s


module Response = struct
  open Ezjsonm
  type t = response

  let obj o : t = `O o

  let make ~id ~name x : t = obj [
      "id", string id;
      name, x
    ]

  let error id sev desc : t =
    make ~id ~name:"error" @@ dict [
      "severity", string (string_of_severity sev);
      "description", string desc
    ]

  let resource ~id links props : t = obj @@ [
      "id", string id;
      "links", strings @@ List.map ~f:Uri.to_string links
    ] @ props

  let disassembler
      ~name ~arch ~kinds ~has_name ~has_ops ~has_target ~has_bil : t =
    obj [
      "name", string name;
      "architecture", string arch;
      "kinds", strings @@ Adt.strings_of_kinds kinds;
      "has-name", bool has_name;
      "has-ops", bool has_ops;
      "has-target", bool has_target;
      "has-bil", bool has_bil
    ]


  let string_of_sym s =
    Sexp.to_string (<:sexp_of<[`debug | `symtab]>> s)
  let strings_of_syms syms =
    List.intersperse ~sep:"," @@ List.map syms ~f:string_of_sym

  let loader ~name ~arch ~format syms : t =
    obj [
      "name", string name;
      "architecture", string arch;
      "format", string format;
      "symbols", strings (strings_of_syms syms)
    ]


  let optional_field name json_of_value = function
    | None -> []
    | Some value -> [name, json_of_value value]

  let num x = string @@ Int.to_string x
  let enum map x = strings (map x)

  let memory ?section ?symbol ~id links mem : t =
    let size =
      Addr.Int_exn.(Memory.(max_addr mem - min_addr mem)) |>
      Addr.to_int |> function
      | Ok size -> Int.to_string size
      | Error err -> Error.raise @@ Error.tag_arg err
          "unable to get memory size" mem sexp_of_mem in
    let addr =              (* json doesn't understand hexes *)
      Memory.max_addr mem |> Addr.string_of_value ~hex:false in
    resource ~id links @@ [
      "addr", string addr;
      "size", string size;
    ] @ optional_field "section" num section
      @ optional_field "symbol"  num symbol


  let basic_insn ~mem_id insn =
    let open Disasm.Basic in
    dict @@ [
      "memory", string @@ Int.to_string mem_id;
      "name", string @@ Insn.name insn;
      "asm", string @@ Insn.asm insn;
      "kinds", strings @@ Adt.strings_of_kinds @@ Insn.kinds insn;
      "operands", strings @@ Adt.strings_of_ops
      @@ Array.to_list @@ Insn.ops insn;
    ]

end
