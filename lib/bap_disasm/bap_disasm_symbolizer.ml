open Core_kernel
open Bap_core_theory
open Bap_types.Std
open Bap_image_std
open Bap_disasm_source
open KB.Syntax


type t = Symbolizer of (addr -> string option)
type symbolizer = t

let name_choices = KB.Domain.opinions ~empty:None
    ~equal:(Option.equal String.equal)
    ~inspect:(sexp_of_option sexp_of_string)
    "name-choices"

let common_name =
  KB.Class.property ~package:"bap.std"
    Theory.Program.cls "common-name" name_choices


let name_of_addr addr =
  sprintf "sub_%s" @@ Addr.string_of_value addr

module Name = struct
  let is_empty name =
    String.is_prefix name ~prefix:"sub_"

  let order x y : KB.Order.partial =
    match is_empty x, is_empty y with
    | true,true -> EQ
    | false,false -> if String.equal x y then EQ else NC
    | true,false -> LT
    | false,true -> GT
end

let create fn = Symbolizer fn

let run (Symbolizer f) a = f a
let resolve sym addr = match run sym addr with
  | Some name -> name
  | None -> name_of_addr addr

let empty = create (fun _ -> None)

let chain ss =
  create (fun addr -> List.find_map ss ~f:(fun s -> run s addr))

let of_image img =
  let symtab = Image.symbols img in
  let names = Addr.Table.create () in
  Table.iteri symtab ~f:(fun mem sym ->
      let name = Image.Symbol.name sym
      and addr = Memory.min_addr mem in
      if not (Name.is_empty name)
      then Hashtbl.set names ~key:addr ~data:name);
  create (Hashtbl.find names)

let of_blocks seq =
  let syms = Addr.Table.create () in
  Seq.iter seq ~f:(fun (name,addr,_) ->
      Hashtbl.set syms ~key:addr ~data:name);
  create (Hashtbl.find syms)

module Factory = Factory.Make(struct type nonrec t = t end)

let provide agent (Symbolizer name) =
  let open KB.Syntax in
  KB.propose agent common_name @@ fun label ->
  KB.collect Arch.slot label >>= fun arch ->
  KB.collect Theory.Label.addr label >>| fun addr ->
  match arch, addr with
  | Some arch, Some addr ->
    let width = Size.in_bits (Arch.addr_size arch) in
    name (Addr.create addr width)
  | _ -> None


let update_name_slot label name =
  KB.collect Theory.Label.name label >>= function
  | Some _ -> KB.return ()
  | None ->
    KB.provide Theory.Label.name label (Some name)

let get_name addr =
  let data = Some (Word.to_bitvec addr) in
  KB.Object.scoped Theory.Program.cls @@ fun label ->
  KB.provide Theory.Label.addr label data >>= fun () ->
  KB.resolve common_name label >>= function
  | Some name -> KB.return name
  | None -> KB.collect Theory.Label.name label >>| function
    | Some name -> name
    | None -> name_of_addr addr

module Toplevel = struct
  let name = Toplevel.var "symbol-name"

  let get_name addr =
    Toplevel.put name (get_name addr);
    Toplevel.get name
end
