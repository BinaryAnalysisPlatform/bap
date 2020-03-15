open Core_kernel
open Bap.Std
open Bap_core_theory
open Bap_knowledge
include Self ()

open Bap_main
open KB.Syntax

type t = {
  aliases : tid option String.Map.t;
  symbols : tid String.Map.t;
}

let empty = {
  aliases = Map.empty (module String);
  symbols = Map.empty (module String);
}

let is_stub sub =
  KB.collect (Value.Tag.slot Sub.stub) (Term.tid sub) >>= function
  | None -> KB.return false
  | Some () -> KB.return true

let at_most_one t key data =
  Map.update t key ~f:(function
      | None -> Some data
      | Some _ -> None)

let add_alias t name s = at_most_one t name (Term.tid s)

let update_aliases aliases sub =
  KB.collect Theory.Label.aliases (Term.tid sub) >>|
  Set.fold ~init:aliases ~f:(fun als name -> add_alias als name sub)

let partition prog =
  Term.to_sequence sub_t prog |>
  Knowledge.Seq.fold ~init:empty
    ~f:(fun {symbols; aliases} s ->
        is_stub s >>= fun is_stub ->
        if is_stub then
          update_aliases aliases s >>= fun aliases ->
          KB.return {symbols; aliases}
        else
          let symbols = Map.set symbols (Sub.name s) (Term.tid s) in
          KB.return {symbols; aliases})

let resolve prog =
  partition prog >>= fun {symbols; aliases} ->
  Map.fold aliases
    ~init:(Map.empty (module Tid))
    ~f:(fun ~key:alias ~data:stub_tid links ->
        match Map.find symbols alias, stub_tid with
        | Some tid', Some stub_tid -> at_most_one links stub_tid tid'
        | _ -> links) |>
  KB.return

let tids = Knowledge.Domain.mapping (module Tid) "tids"
    ~equal:(Option.equal Tid.equal)
    ~inspect:(Option.sexp_of_t sexp_of_tid)

let slot = Knowledge.Class.property
    Theory.Program.cls "stubs"
    tids
    ~persistent:(Knowledge.Persistent.of_binable (module struct
                   type t = tid option Tid.Map.t
                   [@@deriving bin_io]
                 end))
    ~public:true
    ~desc:"The mapping from stubs to real symbols"

let cls = Knowledge.Slot.cls slot

let provide prog =
  Knowledge.Object.create cls >>= fun obj ->
  resolve prog >>= fun links ->
  KB.provide slot obj links >>= fun () ->
  KB.return obj

let find_links prog =
  match Knowledge.run cls (provide prog) (Toplevel.current ()) with
  | Ok (v,_) -> Knowledge.Value.get slot v
  | Error cnf ->
    error "%a\n" Knowledge.Conflict.pp cnf;
    Map.empty (module Tid)

let relink prog links =
  (object
    inherit Term.mapper

    method! map_jmp jmp =
      match Jmp.alt jmp with
      | None -> jmp
      | Some alt -> match Jmp.resolve alt with
        | Second _ -> jmp
        | First tid -> match Map.find links tid with
          | Some (Some tid') ->
            Jmp.reify
              ?cnd:(Jmp.guard jmp)
              ?dst:(Jmp.dst jmp)
              ~alt:(Jmp.resolved tid')
              ~tid:(Term.tid jmp)
              ()
          | _ -> jmp
  end)#run prog

module Plt = struct

  let is_section name v =
    match Value.get Image.section v with
    | Some x -> String.(x = name)
    | _ -> false

  let section_memory proj sec_name =
    let collect_addresses addrs (mem,_ )=
      Memory.foldi mem ~word_size:`r8 ~init:addrs
        ~f:(fun addr _ acc -> Set.add acc (Word.to_bitvec addr)) in
    Memmap.filter (Project.memory proj) ~f:(is_section sec_name) |>
    Memmap.to_sequence |>
    Seq.fold ~init:(Set.empty (module Bitvec_order)) ~f:collect_addresses

  let provide proj =
    let stubs = section_memory proj ".plt" in
    KB.promise (Value.Tag.slot Sub.stub) @@ fun label ->
    KB.collect Theory.Label.addr label >>| function
    | Some addr when Set.mem stubs addr -> Some ()
    | _ ->    None
end

let update prog = relink prog (find_links prog)

let main proj =
  Plt.provide proj;
  Project.with_program proj (update @@ Project.program proj)

let () = Extension.documentation {|
  # DESCRIPTION

  Provides an abi pass that transforms a program by substituting calls
  to stubs with calls to real subroutines when they are present in
  the binary.

|}


let () = Extension.declare @@ fun _ctxt ->
  Bap_abi.register_pass main;
  Ok ()
