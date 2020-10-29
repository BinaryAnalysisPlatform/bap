let doc = "
# DESCRIPTION

Extracts symbolic information from the program relocations.

The relocation symbolizer leverages the relocation information stored
in files to extract symbol names. Since a relocation references an
external symbol which doesn't have an address we use an address of a
callsite.

# SEE ALSO

$(b,bap-plugin-llvm)(1)
"

open Bap_main
open Bap_knowledge
open Core_kernel
open Bap.Std
open Bap_core_theory

include Loggers()

let arch =
  let open Ogre.Syntax in
  Ogre.request Image.Scheme.arch >>| function
  | None -> `unknown
  | Some arch -> match Arch.of_string arch with
    | None -> `unknown
    | Some arch -> arch

let width = Ogre.(arch >>| Arch.addr_size >>| Size.in_bits)

module Bitvec = struct
  include Bitvec
  include Bitvec_sexp.Functions
end

type ref =
  | Addr of Bitvec.t
  | Name of string
[@@deriving compare, sexp, equal]


module References : sig
  type t
  val slot : (Theory.Unit.cls,t) KB.slot
  val lookup : t -> Bitvec.t -> ref option
  val search : t -> Word.t -> ref option
  val prepare : unit -> unit
end = struct
  open Image.Scheme
  open Ogre.Syntax

  module Refs = Map.Make(Bitvec)

  type value = Ref of ref | Bad [@@deriving compare, sexp, equal]
  type t = value Refs.t [@@deriving sexp_of, equal]


  let empty = Refs.empty

  let slot = KB.Class.property Theory.Unit.cls "refs"
      ~package:"bap"
      ~public:true
      ~desc:"external references" @@
    KB.Domain.flat ~empty "refs"
      ~inspect:sexp_of_t
      ~equal

  let chop_version s =
    match String.lfindi s ~f:(fun _ -> Char.equal '@') with
    | None | Some 0 -> s
    | Some len -> String.subo ~len s

  let collect init merge map src =
    width >>| Bitvec.modulus >>= fun m ->
    Ogre.collect Ogre.Query.(select (from src)) >>|
    Seq.fold ~init ~f:(fun exts (addr, value) ->
        Map.update exts Bitvec.(int64 addr mod m) ~f:(function
            | None -> map m value
            | Some value' -> merge m value' value))

  let name _ x = Ref (Name (chop_version x))
  and addr m x = Ref (Addr Bitvec.(int64 x mod m))

  let merge_name m x y =
    let y = name m y in
    match x with
    | Bad -> Bad
    | Ref (Addr _) as y -> y
    | Ref (Name _) as x ->
      if compare_value x y = 0 then y else Bad

  let merge_addr m x y =
    let y = addr m y in
    match x with
    | Bad -> Bad
    | Ref (Addr _) as x when compare_value x y <> 0 -> Bad
    | _ -> y

  let extract =
    collect empty merge_name name external_reference >>= fun names ->
    collect names merge_addr addr relocation

  let create doc = match Ogre.eval extract doc with
    | Ok exts -> exts
    | Error err ->
      warning "Failed to obtain external references: %a" Error.pp err;
      empty

  let lookup exts addr = match Map.find exts addr with
    | Some Bad ->
      warning "pruning a reference at %a for it being a bad reference"
        Bitvec.pp addr;
      None
    | Some Ref x -> Some x
    | None -> None
  let search exts addr = lookup exts (Word.to_bitvec addr)

  let () =
    let open KB.Rule in
    declare ~package:"bap" "refs-of-spec" |>
    require Image.Spec.slot |>
    provide slot |>
    comment "extracts external references from the specification"

  let prepare () =
    let open KB.Syntax in
    KB.promise slot @@ fun unit ->
    KB.collect Image.Spec.slot unit >>| create
end

let plt_agent = Knowledge.Agent.register
    ~package:"bap" "plt-symbolizer"
    ~desc:"extracts symbols from external relocations"

open KB.Syntax

let optimize = Bil.fixpoint @@ Fn.compose
    Bil.propagate_consts
    Bil.fold_consts

let collect_insns number_of_instructions entry =
  let return bils = KB.return @@
    optimize @@ List.(concat @@ rev bils) in
  let rec collect bils addr collected =
    if collected < number_of_instructions then
      Theory.Label.for_addr addr >>= fun label ->
      KB.collect Theory.Semantics.slot label >>= fun insn ->
      match Insn.bil insn with
      | [Bil.Jmp (Int next)] as bil ->
        collect (bil :: bils) (Addr.to_bitvec next) (collected+1)
      | _ ->
        KB.collect Memory.slot label >>= function
        | Some mem ->
          let next = Addr.to_bitvec @@ Addr.succ @@ Memory.max_addr mem in
          collect (Insn.bil insn :: bils) next (collected+1)
        | None -> return bils
    else return bils  in
  collect [] entry 0

let plt_sizes = [
  Arm_target.parent, 4;
  X86_target.parent, 1;
  Bap_powerpc_target.parent, 4;
]

let plt_size label =
  Theory.Label.target label >>| fun t ->
  List.find_map plt_sizes ~f:(fun (p,s) ->
      Option.some_if (Theory.Target.belongs p t) s)

let extract_external s =
  Option.map
    (String.chop_prefix s ~prefix:"external:")
    (fun s -> match String.chop_suffix s ~suffix:"@external" with
       | None -> Name s
       | Some s -> Name s)

let find_references = List.filter_map ~f:(function
    | Bil.Jmp (Load (_,Int dst,_,_))
    | Bil.Jmp (Int dst)
    | Bil.Move (_, Load (_,Int dst,_,_)) -> Some (Addr (Word.to_bitvec dst))
    | Bil.Special dst -> extract_external dst
    | _ -> None)

let addresses mem =
  let start = Memory.min_addr mem in
  let len = Memory.length mem in
  Seq.init len ~f:(Addr.nsucc start) |>
  Seq.map ~f:Word.to_bitvec

let matches refs ref mem = match ref with
  | Some Name _ as ref -> ref
  | ref ->
    let addr = match ref with
      | Some (Addr addr) -> Seq.singleton addr
      | _ -> Seq.empty in
    Seq.append addr (addresses mem) |>
    Seq.find_map ~f:(References.lookup refs)

let resolve_stubs () =
  KB.propose plt_agent Theory.Label.possible_name @@ fun label ->
  KB.collect Theory.Label.unit label >>=? fun unit ->
  KB.collect References.slot unit >>= fun refs ->
  KB.collect Theory.Label.addr label >>=? fun addr ->
  KB.collect (Value.Tag.slot Sub.stub) label >>= fun is_stub ->
  if not (Option.is_some is_stub) then KB.return None
  else match References.lookup refs addr with
    | Some (Name s) -> KB.return (Some s)
    | _ ->
      plt_size label >>=? fun size ->
      collect_insns size addr >>| fun bil ->
      find_references bil |>
      List.find_map ~f:(function
          | Name s -> Some s
          | Addr dst -> match References.lookup refs dst with
            | Some (Name s) -> Some s
            | _ -> None)

let label_for_ref = function
  | Name s -> Theory.Label.for_name s
  | Addr x -> Theory.Label.for_addr x

let mark_mips_stubs_as_functions () : unit =
  KB.promise Theory.Label.is_subroutine @@ fun label ->
  KB.collect Theory.Label.addr label >>=? fun addr ->
  KB.collect Theory.Label.unit label >>=? fun unit ->
  KB.collect References.slot unit >>= fun refs ->
  KB.collect Theory.Unit.target unit >>| fun target ->
  let is_entry = (Theory.Target.matches target "mips") &&
                 Option.is_some (References.lookup refs addr) in
  Option.some_if is_entry true

let () = Extension.declare ~doc @@ fun _ctxt ->
  References.prepare ();
  resolve_stubs ();
  mark_mips_stubs_as_functions ();
  KB.Rule.(declare ~package:"bap" "roots-for-mips" |>
           require Theory.Label.addr |>
           require Theory.Label.unit |>
           require Theory.Unit.target |>
           require References.slot |>
           provide Theory.Label.is_subroutine |>
           comment "marks external-references as function starts on MIPS");
  KB.Rule.(declare ~package:"bap" "resolve-stubs" |>
           require Theory.Label.addr |>
           require Theory.Label.unit |>
           require References.slot |>
           provide Theory.Label.possible_name |>
           comment "analyzes stubs for external references");
  Ok ()
