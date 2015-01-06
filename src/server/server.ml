open Core_kernel.Std
open Core_lwt.Std
open Lwt_log
open Bap.Std
open Rpc

module Res = Manager
module Dis = Disasm.Basic

let version = "0.1"

module type Disasms = sig
  val get
    : ?cpu:string -> backend:string -> string
    -> f:((Dis.asm, Dis.kinds) Dis.t -> 'a Lwt.Or_error.t)
    -> 'a Lwt.Or_error.t
end

module Disasms : Disasms = struct
  type spec = {
    target : string;
    backend : string;
    cpu : string option;
  } with compare, sexp, fields

  let disasms = 8

  module Spec = Hashable.Make(struct
      type t = spec with compare, sexp
      let hash = Hashtbl.hash
    end)
  module Pools = Spec.Table

  let ds = Pools.create ()

  let rec get ?cpu ~backend target ~f =
    let spec = {target; backend; cpu} in
    match Pools.find ds spec with
    | Some pool -> Lwt.Pool.use pool ~f:(fun dis -> Lwt.return dis >>=? f)
    | None ->
      let pool = Lwt.Pool.create disasms (fun () ->
          Lwt.return @@ Dis.create ?cpu ~backend target >>=? fun dis ->
          let dis = Dis.(dis |> store_asm |> store_kinds) in
          Lwt.Or_error.return dis) in
      Pools.add_exn ds ~key:spec ~data:pool;
      get ?cpu ~backend target ~f
end

let section = Lwt_log.Section.make "bap_server"

let stub name = Lwt.Or_error.unimplemented name

module Handlers(Ctxt : sig
                  val reply : Response.msg -> unit Lwt.t
                end) = struct
  open Ctxt

  let reply_error sev fmt =
    Printf.ksprintf (fun msg -> Response.error sev msg |> reply) fmt
  let error fmt = reply_error `Error fmt
  let warning fmt = reply_error `Warning fmt

  let init version =
    let ts = List.(Transport.registered_fetchers >>|
                   Response.transport) in
    let kinds = Disasm.Insn.Kind.all in
    let ds =
      Response.disassembler
        ~name:"llvm" ~arch:Arch.ARM ~kinds
        ~has_name:true ~has_bil:true ~has_ops:true ~has_target:true ::
      List.map [Arch.X86_32; Arch.X86_64] ~f:(fun arch ->
          Response.disassembler ~name:"llvm" ~arch
            ~has_name:true ~has_ops:true ~kinds
            ~has_target:false ~has_bil:false) in
    let ls =
      List.map Arch.all ~f:(fun arch ->
          Response.loader ~name:"bap_elf" ~arch
            ~format:"ELF" [`debug]) in
    let capabilities = Response.capabilities ~version ts ls ds in
    let (%) x f = List.map x ~f:Manager.string_of_id |> f in
    let images = Manager.images % Response.images in
    let sections = Manager.sections % Response.sections in
    let symbols = Manager.symbols % Response.symbols in
    let chunks = Manager.chunks % Response.chunks in
    Lwt.List.iter ~f:reply
      [capabilities; images; sections; symbols; chunks] >>=
    Lwt.Or_error.return


  let reply_resource uri res =
    res >>|? Res.string_of_id >>|? Response.added >>= function
    | Ok msg -> reply msg >>= Lwt.Or_error.return
    | Error err -> error "Failed to add resource from %s: %s"
                     (Uri.to_string uri)
                     (Error.to_string_hum err) >>=
      Lwt.Or_error.return


  let load_file ?loader uri =
    Res.add_file ?backend:loader uri |> reply_resource uri

  let load_chunk addr arch endian uri : unit Lwt.Or_error.t =
    Res.add_memory arch endian addr uri |> reply_resource uri


  let get_mem mem : 'a Rpc.resource Lwt.Or_error.t =
    Res.fetch_memory mem >>|? fun m ->
    Res.links_of_memory mem, m

  (** Runs disassembler on the specified memory  *)
  let disasm_mem lift dis ~stop_on (links,mem) : unit Lwt.t=
    let invalid_mem mem =
      error "%s doesn't contain a valid instruction"
        (Sexp.to_string (sexp_of_mem mem)) in
    let emit_insns = Lwt.List.filter_map ~f:(function
        | mem,None -> invalid_mem mem >>= fun () -> return None
        | mem,Some insn ->
          lift mem insn >>= fun (target,bil) ->
          let resp = Response.insn ?target ?bil (links,mem) insn in
          return (Some resp)) in
    Dis.run dis mem ~return ~init:[] ~stop_on
      ~stopped:(fun s _ ->
          warning "Hit end of data before the stop condition"
          >>= fun () -> emit_insns (Dis.insns s))
      ~invalid:(fun s mem _ ->
          invalid_mem mem >>= fun () -> emit_insns (Dis.insns s))
      ~hit:(fun s _ _ _ -> emit_insns (Dis.insns s))
    >>| Response.insns >>= reply

  type ('a,'b) lifter =
    mem -> ('a,'b) Dis.insn -> (Target.t option * stmt list option) Lwt.t

  let arm_lifter : ('a,'b) lifter = fun mem insn ->
    let open Disasm in
    let arm = Arm.Insn.create insn in
    let ops = Array.map (Dis.Insn.ops insn) ~f:Arm.Op.create |>
              Array.to_list |> Option.all in
    let target = Option.both arm ops |>
                 Option.map ~f:(Tuple2.uncurry Target.arm) in
    match Arm.Lift.insn mem insn with
    | Ok bil -> return (target, Some bil)
    | Error err ->
      warning "Failed to raise insn %s to BIL: %s"
        (Sexp.to_string (Dis.Insn.sexp_of_t insn))
        (Error.to_string_hum err) >>= fun () ->
      return (target, None)

  let no_lifter : ('a,'b) lifter = fun _ _ -> return (None,None)

  let lifter_of_arch : arch -> ('a,'b) lifter = function
    | Arch.ARM -> arm_lifter
    | _   -> no_lifter

  let get_insns ?(backend="llvm") stop_on res_id =
    Lwt.return @@ Res.id_of_string res_id >>=? fun id ->
    let mems_of_img img =
      Image.sections img |> Table.to_sequence |>
      Seq.map ~f:fst |> Seq.to_list  in
    let chunk r = Res.memory r |> get_mem >>|? List.return in
    let section = chunk in
    let symbol r =
      Res.memory r |> List1.to_list |>
      Lwt.Or_error.List.map ~f:get_mem in
    let image r = Res.image r |> Res.fetch_image >>|? mems_of_img >>|?
      List.map ~f:(fun mem -> Res.links r, mem)  in
    Res.with_resource id ~chunk ~symbol ~section ~image
    >>=? fun ms ->
    let get_arch r = Lwt.Or_error.return (Res.arch r) in
    Res.with_resource id
      ~chunk:get_arch ~symbol:get_arch
      ~section:get_arch ~image:get_arch >>=? fun arch ->
    let lifter = lifter_of_arch arch in
    let target = Arch.(match backend, arch with
        | "llvm", ARM -> Ok "arm"
        | "llvm", X86_32 -> Ok "i386"
        | "llvm", X86_64 -> Ok "x86_64"
        | backend, arch ->
          Or_error.errorf "Unsupported backend+arch combination: %s+%a"
            backend Arch.pps arch) in
    Lwt.return target >>=? Disasms.get ~backend ~f:(fun dis ->
        Lwt.List.iter ms ~f:(disasm_mem lifter dis ~stop_on) >>= fun () ->
        Lwt.Or_error.return ())

  let get_resource res_id : 'a Lwt.Or_error.t =
    Lwt.return @@ Res.id_of_string res_id >>=? fun id ->
    Res.with_resource id
      ~chunk:(fun r ->
          get_mem (Res.memory r) >>|? Response.memory)
      ~symbol:(fun r ->
          let sym = Res.symbol r in
          let mem = Res.memory r in
          let m,ms = List1.hd mem, List1.tl mem |> List1.to_list in
          get_mem m >>=? fun m ->
          Lwt.Or_error.List.map ms ~how:`Parallel ~f:get_mem >>|? fun ms ->
          Response.symbol sym (List1.create m ms))
      ~image:(fun r ->
          let img = Res.image r in
          let img_id = Res.id r in
          let links = Res.links_of_image img in
          let secs = Res.sections_of_image img_id |>
                     List.map ~f:Res.string_of_id in
          Res.fetch_image img >>|? Tuple2.create links >>|?
          Response.image ~secs)
      ~section:(fun r ->
          let sec = Res.section r in
          let sec_id = Res.id r in
          let syms = Res.symbols_of_section sec_id |>
                     List.map ~f:Res.string_of_id  in
          let mem = Res.memory r in
          get_mem mem >>|? Response.section ~syms sec) >>=? fun msg ->
    reply msg >>= Lwt.Or_error.return
end

let accept reply (req : request) : unit Lwt.Or_error.t =
  let module H = Handlers(struct let reply = reply end) in
  let open H in
  Request.accept req ~init ~load_file ~load_chunk
    ~get_insns ~get_resource |> Lwt.return |> Lwt.Or_error.join

exception Stopped

let run_exn (request, reply) : unit Lwt.t =
  let reply x = reply x >>= function
    | Ok () -> Lwt.return_unit
    | Error err ->
      error_f "Service has finished with error: %s"
        (Error.to_string_hum err) >>= fun () ->
      Lwt.fail Stopped in
  let handle_request req =
    Request.id req |> Lwt.return >>=? fun id ->
    let reply msg = reply (Response.create id msg) in
    accept reply req in
  Lwt.Stream.iter_s request ~f:(fun req ->
      handle_request req >>= function
      | Ok () -> Lwt.return ()
      | Error err -> match Request.id req with
        | Ok id ->
          let str = Error.to_string_hum err in
          let msg = Response.error `Warning str in
          Response.create id msg |> reply
        | Error err' ->
          let err = Error.of_list [err; err'] in
          let msg = Error.to_string_hum err in
          warning_f ~section "Ignoring junk request: %s" msg)


let run (pipe : (request, response) Transport.pipe)
  : unit Lwt.Or_error.t =
  Lwt.Or_error.try_with (fun () -> run_exn pipe)
