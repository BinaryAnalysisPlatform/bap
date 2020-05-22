open Core_kernel
open Bap.Std
open Bap_c_type
open Monads.Std

module Attrs = Bap_c_term_attributes

type ctype = t

let is_const p = p.Spec.qualifier.Qualifier.const
let is_mutable p = not (is_const p)


let rec lvalue (t : ctype) = match t with
  | `Void -> true
  | `Basic t -> is_mutable t
  | `Pointer ({Spec.t} as p) -> is_mutable p || lvalue t
  | `Array ({Spec.t={Array.element=t}} as p) -> is_mutable p || lvalue t
  | `Function _ -> false
  | `Structure {Spec.t={Compound.fields}}
  | `Union     {Spec.t={Compound.fields}} ->
    List.exists fields ~f:(fun (_,t) -> lvalue t)

let arg_intent : ctype -> intent = function
  | `Void -> In
  | `Basic _ -> In
  | `Pointer {Spec.t} when lvalue t -> Both
  | `Array {Spec.t={Array.element=e}} when lvalue e -> Both
  | `Pointer _ | `Array _ -> In
  | `Function _ -> In
  | `Union _
  | `Structure _ -> In


type error = [
  | `Unknown_interface of string
  | `Parser_error of string * Error.t
] [@@deriving sexp_of]

type param = Bap_c_data.t * exp

type args = {
  return : param option;
  hidden : (Bap_c_type.t * param) list;
  params : param list;
}

type t = {
  insert_args : sub term -> attr list -> proto -> args option;
  apply_attrs : attr list -> sub term -> sub term;
}


exception Failed of error [@@deriving sexp_of]
let fail x = raise (Failed x)

let data (size : #Bap_c_size.base) (t : Bap_c_type.t) =
  let open Bap_c_data in
  let rec data = function
    | `Void -> Seq []
    | `Basic {Spec.t} -> Imm (size#basic t, Top)
    | `Pointer {Spec.t} -> Ptr (data t)
    | `Array {Spec.t={Array.element=t; size=None}} -> Ptr (data t)
    | `Array {Spec.t={Array.element=t; size=Some n}} ->
      let et = data t in
      Ptr (Seq (List.init n ~f:(fun _ -> et)))
    | `Structure {Spec.t={Compound.fields=fs}} ->
      let _,ss =
        List.fold fs ~init:(0,[]) ~f:(fun (off,seq) (_,t) ->
            let off' = match size#bits t with
              | None -> off + Size.in_bits size#pointer (* or assert false *)
              | Some sz -> off + sz in
            match size#padding t off with
            | None ->  off', data t :: seq
            | Some pad -> off, data t :: Imm (pad,Set []) :: seq) in
      Seq (List.rev ss)
    | `Union {Spec.t=_} ->
      let sz = match size#bits t with
        | None -> Size.in_bits size#pointer
        | Some sz -> sz in
      Seq (List.init (sz/8) ~f:(fun _ -> Imm (`r8,Set [])))
    | `Function _ -> Ptr (Imm ((size#pointer :> size),Top)) in
  data t

let create_arg i addr_size intent name t (data,exp) sub =
  let typ = match data with
    | Bap_c_data.Imm (sz,_) -> Type.Imm (Size.in_bits sz)
    | _ -> Type.Imm (Size.in_bits addr_size) in
  let name = if String.is_empty name then sprintf "arg%d" (i+1) else name in
  let var = Var.create (Sub.name sub ^ "_" ^ name) typ in
  let arg = Arg.create ~intent var exp in
  let arg = Term.set_attr arg Attrs.data data in
  let arg = Term.set_attr arg Attrs.t t in
  arg

let registry = Hashtbl.create (module String)
let register name abi = Hashtbl.set registry ~key:name ~data:abi
let get_processor name = Hashtbl.find registry name

let get_prototype gamma name = match gamma name with
  | Some (`Function proto) -> proto
  | _ ->
    let open Bap_c_type in
    Spec.{
      qualifier = `no_qualifier;
      attrs = [];
      t = Proto.{
          args = [];
          variadic = false;
          return = `Basic {
              qualifier = Qualifier.{
                  const = false;
                  volatile = false;
                  restrict = ();
                };
              attrs = [];
              t = `sint;
            }
        }
    }

let create_api_processor size abi : Bap_api.t =
  let addr_size = size#pointer in

  let stage1 gamma = object(self)
    inherit Term.mapper as super
    method! map_sub sub =
      if Term.has_attr sub Attrs.proto then sub
      else self#apply_proto sub

    method private apply_proto sub =
      let name = Sub.name sub in
      let {Bap_c_type.Spec.t; attrs} = get_prototype gamma name in
      let sub = self#apply_args sub attrs t in
      let sub = Term.set_attr sub Attrs.proto t in
      let sub = List.fold_right ~init:sub attrs ~f:Bap_c_attr.apply in
      abi.apply_attrs attrs sub


    method private apply_args sub attrs t =
      match abi.insert_args sub attrs t with
      | None -> super#map_sub sub
      | Some {return; hidden; params} ->
        let params = List.mapi params ~f:(fun i a -> i,a) in
        let args =
          List.map2_exn params t.Bap_c_type.Proto.args ~f:(fun (i,a) (n,t) ->
              create_arg i addr_size (arg_intent t) n t a sub) in
        let ret = match return with
          | None -> []
          | Some ret ->
            let t = t.Bap_c_type.Proto.return in
            [create_arg 0 addr_size Out "result" t ret sub] in
        let hid = List.mapi hidden ~f:(fun i (t,a) ->
            let n = "hidden" ^ if i = 0 then "" else Int.to_string i in
            create_arg 0 addr_size Both n t a sub) in
        List.fold (args@hid@ret) ~init:sub ~f:(Term.append arg_t)

  end in
  let module Api = struct
    let language = "c"
    type t = Term.mapper
    let parse_exn get_api intfs : t =
      let gamma = String.Table.create () in
      List.iter intfs ~f:(fun api ->
          match get_api api with
          | None -> fail (`Unknown_interface api)
          | Some file ->
            match Bap_c_parser.run (size :> Bap_c_size.base) file with
            | Error e -> fail (`Parser_error (api,e))
            | Ok api ->
              List.iter api ~f:(fun (key,t) ->
                  Hashtbl.set gamma ~key ~data:t));
      stage1 (Hashtbl.find gamma)

    let parse get ifs = Or_error.try_with (fun () -> parse_exn get ifs)

    let mapper = ident
  end in
  (module Api)

module Stack = struct
  let create ?(growsup=false) arch off =
    let module Target = (val target_of_arch arch) in
    let sz = (Arch.addr_size arch :> Size.t) in
    let width = Size.in_bits sz in
    let endian = Arch.endian arch in
    let mem = Bil.var Target.CPU.mem in
    let sp = Target.CPU.sp in
    let off = Word.of_int ~width (off * Size.in_bytes sz) in
    let addr = if Word.is_zero off
      then Bil.(var sp)
      else if growsup
      then Bil.(var sp - int off)
      else Bil.(var sp + int off) in
    Bil.load ~mem ~addr endian sz

end
