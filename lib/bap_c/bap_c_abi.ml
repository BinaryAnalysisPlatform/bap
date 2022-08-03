open Core_kernel[@@warning "-D"]
open Bap_core_theory
open Bap.Std
open Bap_c_type
open Monads.Std

include Self()

module Attrs = Bap_c_term_attributes
module Data = Bap_c_data
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

let sexp_of_exp exp = Sexp.Atom (Exp.to_string exp)
type param = Data.t * exp [@@deriving sexp]

type args = {
  return : param option;
  hidden : (Bap_c_type.t * param) list;
  params : param list;
} [@@deriving sexp]

type t = {
  insert_args : sub term -> attr list -> proto -> args option;
  apply_attrs : attr list -> sub term -> sub term;
}


exception Failed of error [@@deriving sexp_of]
let fail x = raise (Failed x)

let data (size : #Bap_c_size.base) (t : Bap_c_type.t) =
  let open Data in
  let sizeof t = match size#bits t with
    | None -> Size.in_bits size#pointer
    | Some s -> s in
  let padding pad : Data.t =
    match Size.of_int_opt pad with
    | Some pad -> Imm (pad,Set [])
    | None ->
      let data : Data.t = Imm (`r8,Set []) in
      Seq (List.init (pad/8) ~f:(Fn.const data)) in
  let rec data = function
    | `Void -> Seq []
    | `Basic {Spec.t} -> Imm (size#basic t, Top)
    | `Pointer {Spec.t} -> Ptr (data t)
    | `Array {Spec.t={Array.element=t; size=None}} -> Ptr (data t)
    | `Array {Spec.t={Array.element=t; size=Some n}} ->
      Ptr (Seq (List.init n ~f:(Fn.const (data t))))
    | `Structure {Spec.t={Compound.fields=fs}} ->
      List.fold fs ~init:(0,0,[]) ~f:(fun (off,total,seq) (_,t) ->
          let fsize = sizeof t in
          let pad = Bap_c_size.padding (size#alignment t) off in
          off + fsize + pad, total + fsize + pad, match pad with
          | 0 -> data t :: seq
          | _ -> data t :: padding pad :: seq) |> fun (_,total,ss) ->
      let fullsize = sizeof t in
      let pad = max 0 (fullsize - total) in
      let ss = if pad = 0 then ss else padding (fullsize-total) :: ss in
      Seq (List.rev ss)
    | `Union _ ->
      let sz = sizeof t in
      Seq (List.init (sz/8) ~f:(fun _ -> Imm (`r8,Top)))
    | `Function _ -> Ptr (Imm ((size#pointer :> size),Top)) in
  data t

let layout (size : #Bap_c_size.base) (t : Bap_c_type.t) =
  let open Data in
  let sizeof t = match size#bits t with
    | None -> Size.in_bits size#pointer
    | Some s -> s in
  let imm size obj : Data.layout = {layout=Imm(size,obj)}
  and ptr {layout=data} : Data.layout = {layout=Ptr data}
  and seq layouts : Data.layout = {
    layout = Seq (List.map layouts ~f:(fun {layout} -> layout))
  } in
  let padding pad : Data.layout = imm pad Undef in
  let rec layout t : Data.layout = match t with
    | `Void -> imm 8 Undef
    | `Basic {Spec.t} -> imm (Size.in_bits (size#basic t)) (Basic t)
    | `Pointer {Spec.t} -> ptr (layout t)
    | `Array {Spec.t={Array.element=t; size=None}} -> ptr (layout t)
    | `Array {Spec.t={Array.element=t; size=Some n}} ->
      ptr (seq (List.init n ~f:(Fn.const (layout t))))
    | `Structure {Spec.t={Compound.fields=fs}} ->
      List.fold fs ~init:(0,0,[]) ~f:(fun (off,total,seq) (name,t) ->
          let fsize = sizeof t in
          let pad = Bap_c_size.padding (size#alignment t) off in
          off + fsize + pad, total + fsize + pad,
          imm fsize (Field (name,layout t)) ::
          match pad with
          | 0 -> seq
          | _ -> padding pad :: seq) |> fun (_,total,ss) ->
      let fullsize = sizeof t in
      let pad = max 0 (fullsize - total) in
      let ss = if pad = 0 then ss else padding (fullsize-total) :: ss in
      seq (List.rev ss)
    | `Union {Spec.t={Compound.fields=fs}} ->
      let total = sizeof t in
      let variants = List.map fs ~f:(fun (name,t) ->
          let fsize = sizeof t in
          let pad = max 0 (total - fsize) in
          let field = imm fsize @@ Field (name, layout t) in
          match pad with
          | 0 -> field
          | _ -> seq [field; padding pad]) in
      imm total (Union variants)
    | `Function _ -> ptr (imm (Size.in_bits (size#pointer)) Undef) in
  layout t

let rec size_of_data size : Data.t -> int = function
  | Imm (size,_) -> Size.in_bits size
  | Seq xs -> List.sum (module Int) ~f:(size_of_data size) xs
  | Ptr _ -> Size.in_bits (size#pointer)

let rec size_of_layout size : Data.layout -> int =
  fun {layout} -> size_of_datum size layout
and size_of_datum size : _ Data.datum -> int = function
  | Imm (size,_) -> size
  | Seq xs -> List.sum (module Int) ~f:(size_of_datum size) xs
  | Ptr _ -> Size.in_bits (size#pointer)

let array_to_pointer (t : ctype) : ctype =
  match t with
  | `Array ({t={element}} as s) -> `Pointer {s with t = element}
  | t -> t

let decay_arrays : proto -> proto = fun proto -> {
    proto with
    return = array_to_pointer proto.return;
    args = List.Assoc.map ~f:array_to_pointer proto.args;
  }

let coerce ltyp rtyp exp = match ltyp,rtyp with
  | Type.Mem _,_| _,Type.Mem _
  | Type.Unk,_ | _, Type.Unk -> exp
  | Imm m, Imm n -> match Int.compare m n with
    | 0 -> exp
    | 1 -> Bil.(cast signed m exp)
    | _ -> Bil.(cast low m exp)


let create_arg size i intent name t (data,exp) sub =
  let layout = match data with
    | Data.Ptr _ ->
      if Bap_c_type.is_pointer t then layout size t
      else layout size (Bap_c_type.pointer t)
    | _ -> layout size t in
  let ltyp = Type.imm (size_of_layout size layout) in
  let rtyp = Type.infer_exn exp in
  let name = if String.is_empty name then sprintf "arg%d" (i+1) else name in
  let var = Var.create (Sub.name sub ^ "_" ^ name) ltyp in
  let arg = Arg.create ~intent var @@ coerce ltyp rtyp exp in
  let arg = Term.set_attr arg Attrs.data data in
  let arg = Term.set_attr arg Attrs.t t in
  let arg = Term.set_attr arg Attrs.layout layout in
  arg



let models = Hashtbl.create (module Theory.Target)

let register_model target model =
  if Hashtbl.mem models target
  then invalid_argf "A data model for target %s is already set"
      (Theory.Target.to_string target) ();
  Hashtbl.add_exn models target (model :> Bap_c_size.base)

let model target = match Hashtbl.find models target with
  | Some m -> m
  | None -> if Theory.Target.bits target = 32
    then new Bap_c_size.base `LP32
    else new Bap_c_size.base `LP64

let registry = Hashtbl.create (module Theory.Target)

let register name abi =
  let target = match Theory.Target.lookup ~package:"bap" name with
    | Some t -> t
    | None -> invalid_argf
                "The name of the abi should be a valid name. Got %s. \
                 See `bap list targets` for the list valid names" name () in
  Hashtbl.add registry ~key:target ~data:abi |> function
  | `Ok -> ()
  | `Duplicate ->
    invalid_argf "The processor for ABI %s is already registered. \
                  Please pick a unique name" name ()
let register_abi = register

let get_processor name =
  match Theory.Target.lookup ~package:"bap" name with
  | None -> None
  | Some t -> Hashtbl.find registry t

let lookup = Hashtbl.find registry


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


let apply_args abi size attrs t sub =
  let t = decay_arrays t in
  match abi.insert_args sub attrs t with
  | None -> sub
  | Some {return; hidden; params} ->
    let params = List.mapi params ~f:(fun i a -> i,a) in
    List.map2 params t.Bap_c_type.Proto.args ~f:(fun (i,a) (n,t) ->
        create_arg size i (arg_intent t) n t a sub) |>
    function
    | Unequal_lengths ->
      error "The ABI processor generated an incorrect number of \
             argument terms for the subroutine %s: %d <> %d"
        (Sub.name sub)
        (List.length params)
        (List.length t.args);
      sub
    | Ok args ->
      let ret = match return with
        | None -> []
        | Some ret ->
          let t = t.Bap_c_type.Proto.return in
          [create_arg size 0 Out "result" t ret sub] in
      let hid = List.mapi hidden ~f:(fun i (t,a) ->
          let n = "hidden" ^ if i = 0 then "" else Int.to_string i in
          create_arg size 0 Both n t a sub) in
      List.fold (args@hid@ret) ~init:sub ~f:(Term.append arg_t)

let apply abi size attrs t sub =
  let sub = apply_args abi size attrs t sub in
  let sub = Term.set_attr sub Attrs.proto t in
  let sub = List.fold_right ~init:sub attrs ~f:Bap_c_attr.apply in
  abi.apply_attrs attrs sub

let create_api_processor size abi : Bap_api.t =
  let stage1 gamma = object(self)
    inherit Term.mapper as super
    method! map_sub sub =
      if Term.has_attr sub Attrs.proto then sub
      else self#apply_proto sub

    method private apply_proto sub =
      if Term.has_attr sub Sub.intrinsic
      then sub
      else
        let name = Sub.name sub in
        let {Bap_c_type.Spec.t; attrs} = get_prototype gamma name in
        apply abi size attrs t sub
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

    let mapper = Fn.id
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


module Arg = struct
  open Core_kernel[@@warning "-D"]
  open Bap_core_theory
  open Bap.Std
  open Monads.Std

  module C = struct
    module Size = Bap_c_size
    module Type = Bap_c_type
    module Data = Data
  end

  module Stack : sig
    type t

    val create : #C.Size.base -> Theory.Target.t -> t option

    val base : t -> var


    (** [slots] returns a list of [(offset,datum,size_in_bits)]
        slots, where offset is properly aligned.
    *)
    val slots : t -> (int * C.Data.t * int) list

    (** [add datum bits] adds [bits] representing [datum] to the
        next available stack slot. The stack is growing downwards. *)
    val add : C.Type.t -> C.Data.t -> int -> t -> t

    (** [push datum bits] prepends [bits] representing [datume] to
        the beginning of the descending stack. *)
    val push : C.Type.t -> C.Data.t -> int -> t -> t

    (** [skip bits] skips the specified number of bits.   *)
    val skip : int -> t -> t

    (** [is_empty] is true if no stack slots were allocated.  *)
    val is_empty : t -> bool
  end = struct
    type info = {
      datum : C.Data.t;
      ctype : C.Type.t;
    }
    type t = {
      base : Var.t;
      data  : (info option * int) Map.M(Int).t;
      align : (info option -> int -> int);
    }

    let bytes bits = (bits - 1) / 8 + 1

    let create (ruler : #C.Size.base) target =
      let min_alignment = max
          (Theory.Target.data_alignment target / 8)
          (Theory.Target.data_addr_size target / 8) in
      let align = function
        | None ->
          C.Size.next_multitude_of ~n:min_alignment
        | Some {ctype} ->
          let m = Size.in_bytes (ruler#alignment ctype) in
          C.Size.next_multitude_of ~n:(max min_alignment m) in
      match Theory.Target.reg target Theory.Role.Register.stack_pointer with
      | None -> None
      | Some sp -> Some {
          base = Var.reify sp;
          data = Map.empty (module Int);
          align;
        }

    let base stack = stack.base

    let slots stack =
      Map.to_alist stack.data |>
      List.filter_map ~f:(fun (off,(info,bits)) ->
          match info with
          | None -> None
          | Some {datum} -> Some (off,datum,bits))


    let singleton entry stack =
      {stack with data = Map.singleton (module Int) 0 entry}

    let append (info,size) stack = match Map.max_elt stack.data with
      | None -> singleton (info,size) stack
      | Some (k,(_,s)) ->
        let k' = stack.align info (k + bytes s) in
        {stack with data = Map.add_exn stack.data k' (info,size)}

    let add ctype datum size = append (Some {ctype; datum},size)
    let skip size  = append (None,size)

    let push ctype datum size stack =
      Map.fold stack.data ~f:(fun ~key:_ ~data stack ->
          append data stack)
        ~init:(singleton (Some {ctype; datum},size) stack)

    let is_empty stack = Map.is_empty stack.data
  end

  module File = struct
    type t = {
      args : exp Map.M(Int).t;
      bits : int;
    }

    let bits self = self.bits

    let deplet self = {self with args = Map.empty (module Int)}

    let pop self = match Map.min_elt self.args with
      | None -> None
      | Some (k,x) ->
        Some ({self with args = Map.remove self.args k},x)

    let popn n self = match Map.min_elt self.args with
      | None -> None
      | Some (k,_) -> match Map.split self.args (k+n-1) with
        | _,None,_ -> None
        | lt,Some (_,x),rt ->
          Some ({self with args = rt}, Map.data lt @ [x])

    let align n self = match Map.min_elt self.args with
      | None -> None
      | Some (k,_) ->
        let k' = C.Size.next_multitude_of ~n k in
        if k = k' then Some (self,())
        else match Map.split self.args k' with
          | _,None,_ -> None
          | _,_,rt when Map.is_empty rt -> None
          | _,Some (k',x),rt ->
            Some ({self with args = Map.add_exn rt k' x},())

    let available {args} = Map.length args

    let of_list =
      List.foldi ~f:(fun pos regs reg -> Map.add_exn regs pos reg)
        ~init:Int.Map.empty


    let of_exps args = {
      args = of_list args;
      bits = match args with
        | [] -> -1
        | r::_ -> match Type.infer_exn r with
          | Imm x -> x
          | _ -> -1
    }

    let create regs = of_exps @@ List.map ~f:Bil.var regs

    let of_roles roles t  =
      let regs =
        Theory.Target.regs t ~roles |>
        Set.to_list |>
        List.map ~f:Var.reify in
      create regs
  end

  type state = {
    files : File.t Map.M(Int).t;
    stack : Stack.t option;
    ruler : C.Size.base;
    where : [`Return | `Inputs | `Hidden];
    return : (C.Data.t * Bil.exp) option;
    inputs : (C.Data.t * Bil.exp) list;
    hidden :(C.Type.t * (C.Data.t * Bil.exp)) list;
    target : Theory.Target.t;
  }

  module Arg = struct
    module State = struct type t = state end
    include Monad.State.Make(State)(Monad.Option)
    include Monad.State.T1(State)(Monad.Option)
    let reject : unit -> 'a t = fun () -> lift None
    let catch : 'a t -> (unit -> 'a t) -> 'a t =
      fun x err ->
      let* s = get () in
      match run x s with
      | None -> err ()
      | Some (x,s) ->
        let+ () = put s in
        x
  end

  type semantics = unit
  type arena = int
  type ctype = C.Type.t

  open Arg.Syntax
  open Arg.Let

  module Arena = struct
    let add file =
      let* s = Arg.get () in
      let s = {
        s with files = match Map.max_elt s.files with
          | None -> Map.singleton (module Int) 0 file
          | Some (k,_) ->
            Map.add_exn s.files (k+1) file
      } in
      let+ () = Arg.put s in
      fst (Map.max_elt_exn s.files)

    let create regs = add (File.create (List.map ~f:Var.reify regs))
    let of_exps xs = add (File.of_exps xs)
    let of_roles roles t = add (File.of_roles roles t)

    let iargs = of_roles Theory.Role.Register.[
        function_argument;
        integer;
      ]

    let irets = of_roles Theory.Role.Register.[
        function_return;
        integer;
      ]

    let fargs = of_roles Theory.Role.Register.[
        function_argument;
        floating;
      ]

    let frets = of_roles Theory.Role.Register.[
        function_return;
        floating;
      ]


    let get {files} n = Map.find_exn files n

    let update s n f = match f (get s n) with
      | None -> Arg.reject ()
      | Some (arena,res) ->
        Arg.put {s with files = Map.set s.files n arena} >>= fun () ->
        Arg.return res
    let pop s n = update s n File.pop
    let popn ~n s a = update s a (File.popn n)
    let align ~n s a = update s a (File.align n)
    let deplet s n = update s n @@ fun s -> Some (File.deplet s,())
  end

  let size t =
    let* s = Arg.get () in
    match s.ruler#bits t with
    | None -> Arg.reject ()
    | Some x -> Arg.return x

  let alignment t =
    let+ s = Arg.get () in
    Size.in_bits (s.ruler#alignment t)

  let require cnd = if cnd then Arg.return () else Arg.reject ()

  let push_arg t exp = Arg.update @@ fun s ->
    match s.where with
    | `Return -> {
        s with return = Some (data s.ruler t,exp)
      }
    | `Hidden -> {
        s with hidden = (t, (data s.ruler t, exp)) :: s.hidden
      }
    | `Inputs -> {
        s with inputs = (data s.ruler t, exp) :: s.inputs
      }

  let register file t =
    let* s = Arg.get () in
    let* bits = size t in
    let regs = Arena.get s file in
    require (File.bits regs >= bits) >>= fun () ->
    let* arg = Arena.pop s file in
    push_arg t arg

  let discard ?(n=1) file =
    Arg.get () >>= fun s -> Arena.popn n s file >>| fun _ -> ()

  let registers_for_bits file bits =
    let+ s = Arg.get () in
    let regs = Arena.get s file in
    let abits = File.bits regs in
    if abits > 0
    then Some ((bits - 1) / abits + 1)
    else None

  let count file t =
    let* s = Arg.get () in
    match s.ruler#bits t with
    | None -> Arg.return None
    | Some bits -> registers_for_bits file bits

  let needs_some f = f >>= function
    | None -> Arg.reject ()
    | Some x -> Arg.return x

  let registers_needed file bits =
    needs_some @@ registers_for_bits file bits

  let concat ?(rev=false) xs =
    List.reduce_exn ~f:Bil.concat (if rev then List.rev xs else xs)

  let registers ?rev ?limit file t =
    let* s = Arg.get () in
    let* bits = size t in
    let* regs_needed = registers_needed file bits in
    let limit = Option.value limit ~default:regs_needed in
    require (regs_needed <= limit) >>= fun () ->
    let* args = Arena.popn ~n:regs_needed s file in
    push_arg t @@ concat ?rev args

  let align_even file =
    let* s = Arg.get () in
    Arena.align ~n:2 s file >>| ignore

  let deplet file =
    let* s = Arg.get () in
    Arena.deplet s file

  let switch where = Arg.update @@ fun s -> {s with where}
  let where = Arg.gets @@ fun s -> s.where

  let with_hidden f =
    let* was = where in
    switch `Hidden >>= fun () ->
    let* x = f () in
    switch was >>| fun () ->
    x

  let reference file t =
    with_hidden @@ fun () ->
    register file (C.Type.pointer t)

  let pointer file t =
    register file (C.Type.pointer t)

  let update_stack f =
    let* s = Arg.get () in
    match s.stack with
    | None -> Arg.reject ()
    | Some stack ->
      Arg.put {s with stack = Some (f stack)}

  let push t =
    let* s = Arg.get () in
    let* bits = size t in
    update_stack @@ Stack.add t (data s.ruler t) bits

  let memory t =
    let* s = Arg.get () in
    let* bits = size t in
    update_stack @@ Stack.add t (data s.ruler t) bits

  let hidden t =
    with_hidden @@ fun () ->
    memory (C.Type.pointer t)

  let skip_memory bits = update_stack @@ Stack.skip bits

  let rebase slots =
    let* {target} = Arg.get () in
    skip_memory (slots * Theory.Target.data_addr_size target)

  let load t bits sp base =
    let mem = Var.reify (Theory.Target.data t) in
    let width = Theory.Target.data_addr_size t in
    let addr = function
      | 0 -> Bil.(var sp)
      | off -> Bil.(var sp + int (Word.of_int ~width off)) in
    let endianness =
      if Theory.Endianness.(equal le (Theory.Target.endianness t))
      then LittleEndian else BigEndian in
    let load_byte off =
      Bil.(load ~mem:(var mem) ~addr:(addr off) endianness `r8) in
    let rec load_bytes bytes loaded off =
      if loaded < bits then
        load_bytes (load_byte off :: bytes) (loaded+8) (off+1)
      else
        let bytes = match endianness with
          | LittleEndian -> bytes
          | BigEndian -> List.rev bytes in
        Bil.(cast low) bits (concat bytes) in
    match Size.of_int_opt bits with
    | Some r -> Bil.(load ~mem:(var mem) ~addr:(addr base) endianness r)
    | None -> load_bytes [] 0 base

  let target = Arg.gets @@ fun s -> s.target

  let stack =
    let* s = Arg.get () in
    match s.stack with
    | None -> Arg.reject ()
    | Some stack -> Arg.return stack

  let split_with_memory ?rev ?limit file typ =
    let* s = Arg.get () in
    let* bits = size typ in
    let* needed = registers_needed file bits in
    let regs = Arena.get s file in
    let limit = Option.value limit ~default:(File.available regs) in
    let available = min limit (File.available regs) in
    if available >= needed
    then
      let* args = Arena.popn ~n:needed s file in
      push_arg typ @@ concat  ?rev args
    else
      let* stk = stack in
      let* t = target in
      let base = Stack.base stk in
      let mbits = bits - available * File.bits regs in
      require (Stack.is_empty stk && available > 0) >>= fun () ->
      let* regs = Arena.popn ~n:available s file in
      skip_memory mbits >>= fun () ->
      push_arg typ @@ concat ?rev (regs@[load t mbits base 0])

  let popn_bits arena bits =
    let* s = Arg.get () in
    let* n = registers_needed arena bits in
    Arena.popn ~n s arena

  let is_even x = x land 1 = 0

  let split f1 f2 typ =
    let* bits = size typ in
    let* regs1 = popn_bits f1 (bits/2) in
    let* regs2 = popn_bits f2 (bits/2) in
    require (is_even bits) >>= fun () ->
    push_arg typ @@ concat (regs1@regs2)

  let either op1 op2 = Arg.catch op1 (fun _ -> op2)
  let (<|>) = either

  let choice options =
    match List.reduce ~f:either options with
    | Some option -> option
    | None -> Arg.reject ()


  let define ?(return=Arg.return ()) inputs = Arg.sequence [
      switch `Return;
      return;
      switch `Inputs;
      inputs;
    ]

  let reify target ruler grammar : args option =
    let ruler = (ruler :> Bap_c_size.base) in
    let init = {
      stack = Stack.create ruler target;
      ruler;
      files = Map.empty (module Int);
      where = `Inputs;
      return = None;
      inputs = [];
      hidden = [];
      target;
    } in
    match Arg.run grammar init with
    | None -> None
    | Some ((),{stack;return;inputs;hidden}) ->
      let memory = match stack with
        | None -> []
        | Some stack ->
          let base = Stack.base stack in
          Stack.slots stack |>
          List.map ~f:(fun (off,data,bits) ->
              data,load target bits base off) in
      Some {
        return;
        params = List.rev_append inputs memory;
        hidden = List.rev hidden;
      }

  let unless cnd prog = if not cnd then prog else Arg.return ()
  let on cnd prog = if cnd then prog else Arg.return ()
  let guard = require
  let accept = Arg.return
  let pure = Arg.return
  let zero = Arg.reject

  let install target ruler pass =
    let open Bap_core_theory in
    let abi_name = KB.Name.unqualified (Theory.Target.name target) in
    let abi_processor = {
      apply_attrs = (fun _ x -> x);
      insert_args = fun _ attrs proto ->
        reify target ruler (pass attrs proto)
    } in
    register_abi abi_name abi_processor;
    register_model target ruler;
    Bap_abi.register_pass @@ fun proj ->
    if Theory.Target.equal (Project.target proj) target
    then begin
      Bap_api.process (create_api_processor ruler abi_processor);
      Project.set proj Bap_abi.name abi_name
    end
    else proj

  module Language = struct
    type predicate = ctype -> bool
    type statement = ctype -> unit Arg.t
    type predicates = predicate list
    type statements = statement list
    type command = predicate * statement
    type commands = command list
    type 'a cls = [>] as 'a

    module type V1 = sig
      val install : Theory.Target.t -> #Bap_c_size.base ->
        ((?finish:(unit Arg.t) ->
          return:(alignment:int -> int -> statement) ->
          (alignment:int -> int -> statement) -> unit Arg.t) ->
         unit Arg.t) ->
        unit

      val sequence : commands -> statement
      val select : commands -> statement
      val case : (ctype -> 'a cls Arg.t) -> ('a cls * statement) list -> statement
      val any : predicates -> predicate
      val all : predicates -> predicate
      val neither : predicates -> predicate

      val is : bool -> predicate
      val otherwise : predicate
      val always : predicate
      val never : predicate
      val choose : statements -> statement
      val combine : statements -> statement

      include Monad.Syntax.S with type 'a t := 'a Arg.t
      include Monad.Syntax.Let.S with type 'a t := 'a Arg.t

    end
    module V1 : V1 = struct

      let sequence cmds arg =
        Arg.List.iter cmds ~f:(fun (cnd,action) ->
            if cnd arg then action arg else Arg.return ())

      let select options arg =
        List.find_map options ~f:(fun (cnd,action) ->
            if cnd arg then Some (action arg) else None) |> function
        | Some action -> action
        | None -> Arg.reject ()


      let case
        : (ctype -> 'a cls Arg.t) -> ('a cls * statement) list -> statement
        = fun classify cmds arg ->
          let* cls = classify arg in
          List.find_map cmds ~f:(fun (case,cmd) ->
              Option.some_if (Poly.equal cls case) cmd) |> function
          | None -> Arg.reject ()
          | Some cmd -> cmd arg

      let is cnd = const cnd
      let otherwise = is true
      let any ps x = List.exists ps ~f:(fun p -> p x)
      let all ps x = List.for_all ps ~f:(fun p -> p x)
      let neither ps x = List.for_all ps ~f:(fun p -> not (p x))

      let choose options arg =
        choice (List.map options ~f:(fun f -> f arg))

      let otherwise = Fn.const true
      let always = Fn.const true
      let never = Fn.const false

      let combine xs arg = Arg.List.iter xs ~f:(fun x -> x arg)


      let install
        : Theory.Target.t -> #Bap_c_size.base ->
          ((?finish:unit Arg.t ->
            return:(alignment:int -> int -> statement) ->
            (alignment:int -> int -> statement) -> unit Arg.t) ->
           unit Arg.t) ->
          unit =
        fun target data k ->
        install target data @@ fun _ {Bap_c_type.Proto.return=r; args=xs} ->
        k @@ fun ?(finish=Arg.return ()) ~return args ->
        let return = match r with
          | `Void -> Arg.return ()
          | _ ->
            let* size = size r in
            let* alignment = alignment r in
            return ~alignment size r in
        let inputs = Arg.List.iter xs ~f:(fun (_,t) ->
            let* size = size t in
            let* alignment = alignment t in
            args ~alignment size t) in
        Arg.sequence [
          switch `Return;
          return;
          switch `Inputs;
          inputs;
          finish;
        ]


      include Arg.Syntax
      include Arg.Let
    end

    include V1
  end
  include Arg
  let reject = Arg.reject
end

let define = Arg.install
