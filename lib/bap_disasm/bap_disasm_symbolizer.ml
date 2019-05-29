open Core_kernel
open Bap_core_theory
open Bap_types.Std
open Bap_image_std
open Bap_disasm_source

type t = Symbolizer of (addr -> string option)
type symbolizer = t


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
  Table.iteri symtab ~f:(fun mem name ->
      Hashtbl.set names
        ~key:(Memory.min_addr mem)
        ~data:name);
  let find addr = match Hashtbl.find names addr with
    | None -> None
    | Some sym -> Some (Image.Symbol.name sym) in
  create find

let of_blocks seq =
  let syms = Addr.Table.create () in
  Seq.iter seq ~f:(fun (name,addr,_) ->
      Hashtbl.set syms ~key:addr ~data:name);
  create (Hashtbl.find syms)

module Factory = Factory.Make(struct type nonrec t = t end)

let provide (Symbolizer name) =
  let open KB.Syntax in
  KB.promise Theory.Label.name @@ fun label ->
  KB.collect Arch.slot label >>= fun arch ->
  KB.collect Theory.Label.addr label >>| fun addr ->
  match arch, addr with
  | Some arch, Some addr ->
    let width = Size.in_bits (Arch.addr_size arch) in
    name (Addr.create addr width)
  | _ -> None
