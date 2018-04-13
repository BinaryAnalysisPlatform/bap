open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_source

type t = Symbolizer of (addr -> string option)
type symbolizer = t


let name_of_addr addr =
  sprintf "sub_%s" @@ Addr.string_of_value addr

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

let service = Bap_service.Service.declare "symbolizer"
    ~desc:"A symbolizer service"
    ~uuid:"aa15e6c7-7127-4b69-9138-f23429ce4308"

module Factory = struct
  include Factory.Make(struct type nonrec t = t end)

  let register name source =
    let provider =
      Bap_service.Provider.declare ~desc:"no description provided"
        name service in
    provide provider source
end

let internal_image_symbolizer = (fun img -> Some (of_image img))
