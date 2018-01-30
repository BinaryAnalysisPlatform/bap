open Core_kernel.Std
open Bap.Std
open Bap_future.Std
open Graphlib.Std
open Graphs

include Self ()

module Sources = struct

  let find_source (type t) (module F : Source.Factory.S with type t = t)
      field o = Option.(field o >>= F.find)

  let merge_streams ss ~f : 'a Source.t =
    let stream, signal = Stream.create () in
    List.iter ss ~f:(fun s -> Stream.observe s (fun x -> Signal.send signal x));
    let pair x = Some x, Some x in
    Stream.parse stream ~init:None
      ~f:(fun prev curr -> match curr, prev with
          | Ok curr, None -> pair (Ok curr)
          | Ok curr, Some (Ok prev) -> pair (Ok (f prev curr))
          | Ok _, Some (Error e)
          | Error e, Some (Ok _) -> pair (Error e)
          | Error e, None -> Some (Error e), None
          | Error curr, Some (Error prev) ->
            pair (Error (Error.of_list [prev; curr])))

  let merge_sources create ~f names =
    match List.filter_map names ~f:create with
    | [] -> assert false
    | ss -> merge_streams ss ~f

  let symbolizer =
    let symbolizers = Symbolizer.Factory.list () in
    merge_sources Symbolizer.Factory.find symbolizers ~f:(fun s1 s2 ->
        Symbolizer.chain [s1;s2])

  let rooter =
    let rooters = Rooter.Factory.list () in
    merge_sources Rooter.Factory.find rooters ~f:Rooter.union

end

let dest_of_bil bil =
  (object inherit [word] Stmt.finder
    method! enter_jmp dst goto = match dst with
      | Bil.Int dst -> goto.return (Some dst)
      | _ -> goto
  end)#find bil

let dest fact insn mem =
  let from = Memory.min_addr mem in
  let to_ = Memory.max_addr mem in
  match Rel_fact.find ~from ~to_ (Rel_fact.relocations fact) with
  | Some a -> Some a
  | None ->
    match Rel_fact.find ~from ~to_ (Rel_fact.externals fact) with
    | Some _ -> None
    | None -> dest_of_bil (Insn.bil insn)

let find_calls rels name roots cfg =
  let starts = Addr.Table.create () in
  List.iter roots ~f:(fun addr ->
      Hashtbl.set starts ~key:addr ~data:(name addr));
  Cfg.nodes cfg |> Seq.iter ~f:(fun blk ->
      let mem, term = List.hd_exn @@ List.rev (Block.insns blk) in
      if Insn.(is call) term
      then
        Option.iter (dest rels term mem)
          ~f:(fun w -> Hashtbl.set starts ~key:w ~data:(name w)));
  starts

let reconstruct rels name roots cfg =
  let roots = find_calls rels name roots cfg in
  let init =
    Cfg.nodes cfg |> Seq.fold ~init:Cfg.empty ~f:(fun cfg n ->
        Cfg.Node.insert n cfg) in
  let filtered =
    Cfg.edges cfg |> Seq.fold ~init ~f:(fun cfg e ->
        if Hashtbl.mem roots (Block.addr (Cfg.Edge.dst e)) then cfg
        else
          Cfg.Edge.insert e cfg) in
  let find_block addr =
    Cfg.nodes cfg |> Seq.find ~f:(fun blk ->
        Addr.equal addr (Block.addr blk)) in
  Hashtbl.fold roots ~init:Symtab.empty
    ~f:(fun ~key:entry ~data:name syms ->
        match find_block entry with
        | None -> syms
        | Some entry ->
          let cfg : cfg =
            with_return (fun {return} ->
                Graphlib.depth_first_search (module Cfg)
                  filtered ~start:entry ~init:Cfg.empty
                  ~enter_edge:(fun _ -> Cfg.Edge.insert)
                  ~start_tree:(fun n t ->
                      if Block.equal n entry
                      then Cfg.Node.insert n t
                      else return t)) in
          Symtab.add_symbol syms (name,entry,cfg))

let create spec name roots =
  match Rel_fact.create spec with
  | Error er ->
    error "%a" Error.pp er;
    let default = Reconstructor.default name roots in
    let f cfg = Reconstructor.run default cfg in
    Ok (Reconstructor.create f)
  | Ok fact ->
    let f cfg = reconstruct fact name roots cfg in
    Ok (Reconstructor.create f)

(** TODO: think about this module as a draft.
    This module has a lot of copy-paste code.
    It's decidable, but there is a worse thing here.
    It has an assumption that we can reuse
    rooter and symbolizer. *)
let init () =
  let open Project.Info in
  let open Sources in
  let f spec name rooter = match name, rooter with
    | Ok symb, Ok rooter ->
      let name = Symbolizer.resolve symb in
      let roots = Seq.to_list (Rooter.roots rooter) in
      create spec name roots
    | _ -> failwith "symbolizer or/and rooter was not created" in
  Stream.Variadic.(apply (args spec $ symbolizer $ rooter) ~f)|>
  Reconstructor.Factory.register "relocatable"
