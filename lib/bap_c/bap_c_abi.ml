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

let sexp_of_exp exp = Sexp.Atom (Exp.to_string exp)
type param = Bap_c_data.t * exp [@@deriving sexp]

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
      | None ->
        super#map_sub sub
      | Some {return; hidden; params} ->
        let params = List.mapi params ~f:(fun i a -> i,a) in
        List.map2 params t.Bap_c_type.Proto.args ~f:(fun (i,a) (n,t) ->
            create_arg i addr_size (arg_intent t) n t a sub) |>
        function
        | Unequal_lengths -> super#map_sub sub
        | Ok args ->
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


module Arg = struct
  open Core_kernel
  open Bap_core_theory
  open Bap.Std
  open Monads.Std

  module C = struct
    module Size = Bap_c_size
    module Type = Bap_c_type
    module Data = Bap_c_data
  end

  let next_multitude_of ~n x = (x + (n-1)) land (lnot (n-1))



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
          next_multitude_of ~n:min_alignment
        | Some {ctype} ->
          let m = Size.in_bytes (ruler#alignment ctype) in
          next_multitude_of ~n:(max min_alignment m) in
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
      args : Var.t Map.M(Int).t;
      bits : int;
    }

    let bits self = self.bits

    let deplet self = {self with args = Map.empty (module Int)}

    let pop self = match Map.min_elt self.args with
      | None -> None
      | Some (k,x) ->
        Some ({self with args = Map.remove self.args k},x)

    let popn n self = match Map.split self.args n with
      | _,None,_ -> None
      | lt,Some (k,x),rt ->
        Some ({self with args = Map.add_exn rt k x}, Map.data lt)

    let align n self = match Map.min_elt self.args with
      | None -> None
      | Some (k,_) ->
        let k' = next_multitude_of ~n k in
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


    let create args = {
      args = of_list args;
      bits = match args with
        | [] -> -1
        | r::_ -> match Var.typ r with
          | Imm x -> x
          | _ -> -1
    }

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
    let deplet s n = update s n @@ fun s -> Some (File.deplet s,())
  end

  let size s t = match s.ruler#bits t with
    | None -> Arg.reject ()
    | Some x -> Arg.return x

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
    let* bits = size s t in
    let regs = Arena.get s file in
    require (File.bits regs >= bits) >>= fun () ->
    let* arg = Arena.pop s file in
    push_arg t (Bil.var arg)

  let drop file =
    Arg.get () >>= fun s -> Arena.pop s file >>| fun _ -> ()

  let count file t =
    let+ s = Arg.get () in
    let regs = Arena.get s file in
    let abits = File.bits regs in
    match s.ruler#bits t with
    | Some bits when abits > 0 -> Some ((bits - 1) / abits + 1)
    | _ -> None

  let needs_some f = f >>= function
    | None -> Arg.reject ()
    | Some x -> Arg.return x

  let registers ?limit file t =
    let* s = Arg.get () in
    let* regs_needed = needs_some @@ count file t in
    let limit = Option.value limit ~default:regs_needed in
    require (regs_needed <= limit) >>= fun () ->
    let* args = Arena.popn ~n:regs_needed s file in
    let exps = List.map args ~f:Bil.var |>
               List.reduce_exn ~f:Bil.concat in
    push_arg t exps

  let align_even file =
    let* s = Arg.get () in
    Arena.popn ~n:2 s file >>| ignore

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
    register file (`Pointer C.Type.Spec.{
        t;
        attrs = [];
        qualifier = C.Type.Qualifier.{
            const = false;
            volatile = false;
            restrict = false;
          }
      })

  let update_stack f =
    let* s = Arg.get () in
    match s.stack with
    | None -> Arg.reject ()
    | Some stack ->
      Arg.put {s with stack = Some (f stack)}

  let push t =
    let* s = Arg.get () in
    let* bits = size s t in
    update_stack @@ Stack.add t (data s.ruler t) bits

  let memory t =
    let* s = Arg.get () in
    let* bits = size s t in
    update_stack @@ Stack.add t (data s.ruler t) bits

  let skip_memory bits = update_stack @@ Stack.skip bits

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
        Bil.(cast low) bits (List.reduce_exn bytes ~f:Bil.concat) in
    match Size.of_int_opt bits with
    | Some r -> Bil.(load ~mem:(var mem) ~addr:(addr base) endianness r)
    | None -> load_bytes [] 0 base

  let target = Arg.gets @@ fun s -> s.target

  let stack =
    let* s = Arg.get () in
    match s.stack with
    | None -> Arg.reject ()
    | Some stack -> Arg.return stack

  let split_with_memory file typ =
    let* s = Arg.get () in
    let* bits = size s typ in
    let* reg = Arena.pop s file in
    let* t = target in
    let* stack = stack in
    let regs = Arena.get s file in
    let base = Stack.base stack in
    require (Stack.is_empty stack && bits > File.bits regs) >>= fun () ->
    let mbits = bits - File.bits regs in
    skip_memory mbits >>= fun () ->
    push_arg typ @@ Bil.concat (Bil.var reg) (load t mbits base 0)

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
  include Arg
end

let define target ruler pass =
  let open Bap_core_theory in
  let target_name = Theory.Target.name target in
  let abi_name =
    let abi = Theory.Target.abi target in
    if Theory.Abi.(abi = unknown)
    then Format.asprintf "%a-unknown" KB.Name.pp target_name
    else Format.asprintf "%a" KB.Name.pp target_name in
  let abi_processor = {
    apply_attrs = (fun _ x -> x);
    insert_args = fun _ attrs proto ->
      Arg.reify target ruler (pass attrs proto)
  } in
  register abi_name abi_processor;
  Bap_abi.register_pass @@ fun proj ->
  if Theory.Target.equal (Project.target proj) target
  then begin
    Bap_api.process (create_api_processor ruler abi_processor);
    Project.set proj Bap_abi.name abi_name
  end
  else proj
