open Core_kernel.Std
open Bap.Std
open Bap_c_type


module Attrs = Bap_c_term_attributes

type ctype = t

let is_const p = p.Spec.qualifier.Qualifier.const
let is_mutable p = not (is_const p)


let rec lvalue (t : ctype) = match t with
  | `Void -> true
  | `Basic t -> is_mutable t
  | `Pointer ({Spec.t} as p) -> is_mutable p || lvalue t
  | `Array ({Spec.t=(t,_)} as p) -> is_mutable p || lvalue t
  | `Function _ -> false
  | `Structure {Spec.t=fs} | `Union {Spec.t=fs} ->
    List.exists fs ~f:(fun (_,t) -> lvalue t)

let arg_intent : ctype -> intent = function
  | `Void -> In
  | `Basic _ -> In
  | `Pointer {Spec.t} when lvalue t -> Both
  | `Array {Spec.t=(e,_)} when lvalue e -> Both
  | `Pointer _ | `Array _ -> In
  | `Function _ -> In
  | `Union _
  | `Structure _ -> In


type error = [
  | `Unknown_interface of string
  | `Parser_error of string * Error.t
]

type param = Bap_c_data.t * exp

type proto = {
  return : param option;
  hidden : (Bap_c_type.t * param) list;
  params : param list;
}


let create_proto ?return ?(hidden=[]) params =
  {return; hidden; params}

exception Failed of error
let fail x = raise (Failed x)

let data (size : #Bap_c_size.base) (t : Bap_c_type.t) =
  let open Bap_c_data in
  let rec data = function
    | `Void -> Seq []
    | `Basic {Spec.t} -> Imm (size#basic t, Top)
    | `Pointer {Spec.t} -> Ptr (data t)
    | `Array {Spec.t=(t,None)} -> Ptr (data t)
    | `Array {Spec.t=(t,Some n)} ->
      let et = data t in
      Ptr (Seq (List.init n ~f:(fun _ -> et)))
    | `Structure {Spec.t=fs} ->
      let _,ss =
        List.fold fs ~init:(0,[]) ~f:(fun (off,seq) (_,t) ->
            let off' = match size#bits t with
              | None -> off + Size.in_bits size#pointer (* or assert false *)
              | Some sz -> off + sz in
            match size#padding t off with
            | None ->  off', data t :: seq
            | Some pad -> off, data t :: Imm (pad,Set []) :: seq) in
      Seq (List.rev ss)
    | `Union {Spec.t=fs} ->
      let sz = match size#bits t with
        | None -> Size.in_bits size#pointer
        | Some sz -> sz in
      Seq (List.init (sz/8) ~f:(fun _ -> Imm (`r8,Set [])))
    | `Function _ -> Ptr (Imm ((size#pointer :> size),Top)) in
  data t


let create_arg addr_size intent name t (data,exp) sub =
  let typ = match data with
    | Bap_c_data.Imm (sz,_) -> Type.Imm (Size.in_bits sz)
    | _ -> Type.Imm (Size.in_bits addr_size) in
  let var = Var.create (Sub.name sub ^ "_" ^ name) typ in
  let arg = Arg.create ~intent var exp in
  let arg = Term.set_attr arg Attrs.data data in
  let arg = Term.set_attr arg Attrs.t t in
  arg

let create_api_processor arch args : Bap_api.t =
  let addr_size = Arch.addr_size arch in
  let mapper gamma = object
    inherit Term.mapper as super
    method! map_sub sub =
      let name = Sub.name sub in
      match gamma name with
      | Some (`Function {Bap_c_type.Spec.t}) ->
        let {return; hidden; params} = args t in
        let args =
          List.map2_exn params t.Bap_c_type.Proto.args ~f:(fun a (n,t) ->
              create_arg addr_size (arg_intent t) n t a sub) in
        let ret = match return with
          | None -> []
          | Some ret ->
            let t = t.Bap_c_type.Proto.return in
            [create_arg addr_size Out "result" t ret sub] in
        let hid = List.mapi hidden ~f:(fun i (t,a) ->
            let n = "hidden" ^ if i = 0 then "" else Int.to_string i in
            create_arg addr_size Both n t a sub) in
        List.fold (args@hid@ret) ~init:sub ~f:(Term.append arg_t)
      | _ -> super#map_sub sub
  end in
  let module Api = struct
    let language = "c"
    type t = Term.mapper
    let parse_exn get_api intfs =
      let gamma = String.Table.create () in
      List.iter intfs ~f:(fun api ->
          match get_api api with
          | None -> fail (`Unknown_interface api)
          | Some file ->
            match Bap_c_parser.run file with
            | Error e -> fail (`Parser_error (api,e))
            | Ok api ->
              List.iter api ~f:(fun (key,t) ->
                  Hashtbl.set gamma ~key ~data:t));
      mapper (Hashtbl.find gamma)

    let parse get ifs = Or_error.try_with (fun () -> parse_exn get ifs)

    let mapper = ident
  end in
  (module Api)



module Stack = struct
  let create ?(growsup=false) arch =
    let module Target = (val target_of_arch arch) in
    let sz = (Arch.addr_size arch :> Size.t) in
    let width = Size.in_bits sz in
    let endian = Arch.endian arch in
    let mem = Bil.var Target.CPU.mem in
    let sp = Target.CPU.sp in
    fun off ->
      let off = Word.of_int ~width (off * Size.in_bytes sz) in
      let addr = if Word.is_zero off
        then Bil.(var sp)
        else if growsup
        then Bil.(var sp - int off)
        else Bil.(var sp + int off) in
      Bil.load ~mem ~addr endian sz

end
