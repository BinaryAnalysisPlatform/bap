open Core_kernel.Std
open Bap.Std
open Bil.Types

(**  : tid -> Ir_piqi.tid *)
let piqi_of_tid tid =
  Tid.sexp_of_t tid |> Sexp.to_string

(** *)
let piqi_of_intent = function
    | Some In -> Some Ir_piqi.(`into)
    | Some Out -> Some Ir_piqi.(`out)
    | Some Both -> Some Ir_piqi.(`both)
    | None -> None

let piqi_of_typeid typeid =
  Sexp.to_string @@ Value.Typeid.sexp_of_t typeid

let add_path_table_entry ptable key data =
  { Ir_piqi.Path_table_entry.path_key = key; path = data } :: ptable

let add_exp_map_entry ~key ~data accu =
  { Ir_piqi.Tid_exp_map_entry.tid = piqi_of_tid key ;
    exp = Bil_piqi.exp_to_piqi data } :: accu

let piqi_of_dict dict =
  Seq.to_list @@ Seq.map dict (fun (typeid, value) ->
  { Ir_piqi.Value.typeid = piqi_of_typeid typeid ;
    value = Value.to_string value })

let piqi_of_term aterm =
  let tid = (Term.tid aterm) in
  let dict = (Dict.all_pairs @@ Term.attrs aterm) in
  { Ir_piqi.Term.tid = piqi_of_tid tid ;
    Ir_piqi.Term.dict = piqi_of_dict dict }

let piqi_of_phi phi_term =
  let var, exp_map = Phi.lhs phi_term, Phi.values phi_term in
  let result = [] in
  let add_entry = (fun accu key_data -> let key, data = key_data in
                    add_exp_map_entry ~key ~data accu) in
  let exp_map = Seq.fold exp_map ~init:result ~f:add_entry in
  { Ir_piqi.Phi.var = Bil_piqi.var_to_piqi var;
    exprs = exp_map }

let piqi_of_def def =
  let var, exp = Def.lhs def, Def.rhs def in
  { Ir_piqi.Def.var = Bil_piqi.var_to_piqi var;
    exp = Bil_piqi.exp_to_piqi exp }

let def_term_to_piqi def_term =
  { Ir_piqi.Def_term.term = piqi_of_term def_term;
    self = piqi_of_def def_term}

let piqi_of_label : label -> Ir_piqi.label = function
  | Indirect exp -> (Ir_piqi.(`indirect (Bil_piqi.exp_to_piqi exp) ))
  | Direct tid ->   (Ir_piqi.(`direct (piqi_of_tid tid) ))

let piqi_of_call call =
  { Ir_piqi.Call.target = piqi_of_label @@ Call.target call;
    return = match Call.return call with
      | Some label -> Some (piqi_of_label label)
      | None -> None }

let piqi_of_goto = piqi_of_label
  
let piqi_of_ret = piqi_of_label

let piqi_of_int_tid i tid =
  { Ir_piqi.Int_tid.value = i;
    Ir_piqi.Int_tid.tid = piqi_of_tid tid }    

let piqi_of_jmp_kind : jmp_kind -> Ir_piqi.jmp_kind  = function
  | Call call -> Ir_piqi.(`call (piqi_of_call call))
  | Goto goto -> Ir_piqi.(`goto (piqi_of_goto goto))
  | Ret ret -> Ir_piqi.(`ret (piqi_of_ret ret))
  | Int (i, tid) -> Ir_piqi.(`int_tid (piqi_of_int_tid i tid))

let piqi_of_jmp jmp =
  { Ir_piqi.Jmp.exp = Bil_piqi.exp_to_piqi @@ Jmp.cond jmp;
    Ir_piqi.Jmp.jmp_kind = piqi_of_jmp_kind @@ Jmp.kind jmp }

(*  : blk term -> Ir_piqi.blk *)
let piqi_of_blk blk =
  let phis = Term.enum phi_t blk in
  let defs = Term.enum def_t blk in
  let jmps = Term.enum jmp_t blk in
  { Ir_piqi.Blk.phis
    = Seq.to_list @@ Seq.map phis ~f:(fun phi_term ->
        { Ir_piqi.Phi_term.term
          = piqi_of_term phi_term ;
          self=piqi_of_phi phi_term }) ;
    defs = Seq.to_list @@ Seq.map defs ~f:(fun def_term ->
        { Ir_piqi.Def_term.term = piqi_of_term def_term;
          Ir_piqi.Def_term.self = piqi_of_def def_term });
    jmps = Seq.to_list @@ Seq.map jmps ~f:(fun jmp_term ->
        { Ir_piqi.Jmp_term.term = piqi_of_term jmp_term;
          Ir_piqi.Jmp_term.self = piqi_of_jmp jmp_term }) }

let piqi_of_blk_term blk_term =
  let piqi_blk = piqi_of_blk blk_term in
  let term = piqi_of_term blk_term in
  { Ir_piqi.Blk_term.term = term; self = piqi_blk }

let piqi_of_arg arg = 
  let v, e, i = Arg.lhs arg, Arg.rhs arg, Arg.intent arg in
  let i = piqi_of_intent i in
  { Ir_piqi.Arg.var=Bil_piqi.var_to_piqi v;
                   exp=Bil_piqi.exp_to_piqi e; intent=i }
(*  : arg -> Ir_piqi.arg *)
let piqi_of_arg_term arg_term =
  let piqi_arg = piqi_of_arg arg_term in
  let term = piqi_of_term arg_term in
  { Ir_piqi.Arg_term.term = term ; self = piqi_arg }

let piqi_of_blk_term_array blk_term_array = 
  Seq.to_list
    @@ Seq.map blk_term_array ~f:(fun (blk_term : blk term) ->
        { Ir_piqi.Blk_term.term = (piqi_of_term blk_term);
          self = piqi_of_blk blk_term } )

let piqi_of_arg_term_array arg_term_array =
  Seq.to_list
    @@ Seq.map arg_term_array ~f:(fun arg_term ->
      { Ir_piqi.Arg_term.term = (piqi_of_term arg_term);
        self = piqi_of_arg arg_term })

      
(* : sub -> Ir_piqi.sub *)
let piqi_of_sub s =
  { Ir_piqi.Sub.name = Sub.name s;
    blks = piqi_of_blk_term_array (Term.enum blk_t s);
    args = piqi_of_arg_term_array (Term.enum arg_t s)}

let piqi_of_sub_term sub_term =
  { Ir_piqi.Sub_term.term = piqi_of_term sub_term;
    self = piqi_of_sub sub_term }

(* : phi -> Ir_piqi.phi *)
let piqi_of_phi_term phi =
  { Ir_piqi.Phi_term.term = piqi_of_term phi;
    self = piqi_of_phi phi }

(*  : def -> Ir_piqi.def *)
let piqi_of_def_term def =
  { Ir_piqi.Def_term.term = piqi_of_term def;
    self = piqi_of_def def }

(*  : jmp -> Ir_piqi.jmp *)
let piqi_of_jmp_term jmp = 
  { Ir_piqi.Jmp_term.term = piqi_of_term jmp;
    self = piqi_of_jmp jmp }

(*  : program -> Ir_piqi.program *)
let piqi_of_program_term prog =
  let self = {
    Ir_piqi.Program.subs
    = Seq.to_list @@ Seq.map (Term.enum sub_t prog) ~f:
        piqi_of_sub_term } in
  let term = { Ir_piqi.Term.tid =piqi_of_tid (Term.tid prog) ;
               dict=(piqi_of_dict @@ Dict.all_pairs @@ Term.attrs prog) } in
  { Ir_piqi.Program_term.term=term ; self=self }

(** Conversion from piqi to vernacular types *)
(*TODO every single conversion function has a builder that needs to *)
  (*be initialized with tid. *)
let typeid_of_piqi x =
  Sexp.of_string x |> Value.Typeid.t_of_sexp

let tid_of_piqi x=
  Tid.t_of_sexp @@ Sexp.of_string x

let def_of_piqi piqi_def_term =
  let { Ir_piqi.Def_term.term ; self } = piqi_def_term in
  let { Ir_piqi.Def.var ; exp } = self in
  let var = Bil_piqi.var_of_piqi var in
  let exp = Bil_piqi.exp_of_piqi exp in
  let { Ir_piqi.Term.tid ; _ } = term in
  let tid = tid_of_piqi tid in
  Def.create ~tid var exp  

let intent_of_piqi = function
    | Some `into -> Some In
    | Some `out -> Some Out
    | Some `both -> Some Both
    | None -> None
   
let arg_of_piqi piqi_arg_term =
  let { Ir_piqi.Arg_term.term ; self } = piqi_arg_term in
  let { Ir_piqi.Arg.var ; exp ; intent } = self in
  let var = Bil_piqi.var_of_piqi var in
  let exp = Bil_piqi.exp_of_piqi exp in
  let intent = intent_of_piqi intent in
  let { Ir_piqi.Term.tid ; _ } = term in
  let tid = tid_of_piqi tid in
  Arg.create ~tid ?intent var exp

let phi_of_piqi piqi_phi =
  let { Ir_piqi.Phi_term.term ; self } = piqi_phi in
  let { Ir_piqi.Phi.var ; exprs } = self in
  let var = Bil_piqi.var_of_piqi var in
  let exprs = List.map exprs ~f:(fun tid_exp ->
      let { Ir_piqi.Tid_exp_map_entry.tid; exp } = tid_exp in
      (tid_of_piqi tid), Bil_piqi.exp_of_piqi exp) in
  let { Ir_piqi.Term.tid ; _ } = term in
  let tid = tid_of_piqi tid in
  Phi.of_list ~tid var exprs

let label_of_piqi = function
  | `indirect exp -> Indirect (Bil_piqi.exp_of_piqi exp)
  | `direct tid -> Direct (tid_of_piqi tid)

let call_of_piqi c =
  let { Ir_piqi.Call.target ; return } = c in
  let ret = Option.map return ~f:label_of_piqi in
  Call.create ?return:ret ~target:(label_of_piqi target) ()

let goto_of_piqi = label_of_piqi
let ret_of_piqi  = label_of_piqi
let int_tid_of_piqi piqi_itid =
  let { Ir_piqi.Int_tid.value ; tid } = piqi_itid in
  Bap.Std.Int (value, (tid_of_piqi tid))
  

let jmp_kind_of_piqi : Ir_piqi.jmp_kind -> jmp_kind = function
  | `call c -> Call (call_of_piqi c)
  | `goto g -> Goto (goto_of_piqi g)
  | `ret r -> Ret (ret_of_piqi r)
  | `int_tid piqi_itid -> (int_tid_of_piqi piqi_itid)

let jmp_of_piqi piqi_jmp_term =
  let { Ir_piqi.Jmp_term.term ; self } = piqi_jmp_term in
  let { Ir_piqi.Jmp.exp ; jmp_kind } = self in
  let cond = Bil_piqi.exp_of_piqi exp in
  let jmp_kind = jmp_kind_of_piqi jmp_kind in
  let { Ir_piqi.Term.tid ; _ } = term in
  let tid = tid_of_piqi tid in
  Jmp.create ~tid ~cond jmp_kind
  

let blk_of_piqi piqi_blk_term =
  let { Ir_piqi.Blk_term.term ; self } = piqi_blk_term in
  let { Ir_piqi.Blk.phis ; defs ; jmps } = self in
  let { Ir_piqi.Term.tid ; _ } = term in
  let tid = tid_of_piqi tid in
  let blk_builder = Blk.Builder.create ~tid () in
  let add_phi piqi_phi =
    phi_of_piqi piqi_phi |> Blk.Builder.add_phi blk_builder in
  let add_def piqi_def =
    def_of_piqi piqi_def |> Blk.Builder.add_def blk_builder in
  let add_jmp piqi_jmp =
    jmp_of_piqi piqi_jmp |> Blk.Builder.add_jmp blk_builder in
  List.iter phis ~f:add_phi;
  List.iter defs ~f:add_def;
  List.iter jmps ~f:add_jmp;
  Blk.Builder.result blk_builder
  

let sub_of_piqi piqi_sub_term =
  let { Ir_piqi.Sub_term.term ; self } = piqi_sub_term in
  let { Ir_piqi.Sub.name ; blks ; args } = self in
  let { Ir_piqi.Term.tid ; _ } = term in
  let tid = tid_of_piqi tid in
  let sub_builder = Sub.Builder.create ~tid ~name () in
  let add_blk piqi_blk =
    blk_of_piqi piqi_blk |> Sub.Builder.add_blk sub_builder in
  let add_arg piqi_arg =
    arg_of_piqi piqi_arg |> Sub.Builder.add_arg sub_builder in
  List.iter blks ~f:add_blk;
  List.iter args ~f:add_arg;
  Sub.Builder.result sub_builder

let program_of_piqi piqi_prog =
  let { Ir_piqi.Program_term.term ; self } = piqi_prog in
  let { Ir_piqi.Program.subs } = self in
  let { Ir_piqi.Term.tid ; _ } = term in
  let tid = tid_of_piqi tid in
  let prog_builder = Program.Builder.create ~tid () in
  let add piqi_sub_term =
    (sub_of_piqi piqi_sub_term) |>
    Program.Builder.add_sub prog_builder in
  ignore (List.iter subs ~f:add);
  Program.Builder.result prog_builder

(* Program.t -> string *)
let pb_of_program x =
  Ir_piqi_ext.gen_program_term (piqi_of_program_term x) `pb
(* Blk.t -> string *)
let pb_of_blk x = Ir_piqi_ext.gen_blk_term (piqi_of_blk_term x) `pb
(* Arg.t -> string *)
let pb_of_arg x = Ir_piqi_ext.gen_arg_term (piqi_of_arg_term x) `pb
(* Sub.t -> string *)
let pb_of_sub x = Ir_piqi_ext.gen_sub_term (piqi_of_sub_term x) `pb
(* Phi.t -> string *)
let pb_of_phi x=Ir_piqi_ext.gen_phi_term (piqi_of_phi_term x) `pb
(* Def.t -> string *)
let pb_of_def x = Ir_piqi_ext.gen_def_term (piqi_of_def_term x) `pb
(* Jmp.t -> string *)
let pb_of_jmp x = Ir_piqi_ext.gen_jmp_term (piqi_of_jmp_term x) `pb
(* intent option -> string *)
let pb_of_intent x=
  let x = Option.value_exn (piqi_of_intent x) in Ir_piqi_ext.gen_intent x `pb
(* typeid -> string *)
let pb_of_typeid x =Ir_piqi_ext.gen_typeid (piqi_of_typeid x) `pb
(* call -> string *)
let pb_of_call x= Ir_piqi_ext.gen_call (piqi_of_call x) `pb
(* label -> string *)
let pb_of_label x=Ir_piqi_ext.gen_label (piqi_of_label x) `pb
(* jmp_kind -> string *)
let pb_of_jmp_kind x=Ir_piqi_ext.gen_jmp_kind (piqi_of_jmp_kind x) `pb


(* string -> Program.t *)
let program_of_pb x = Ir_piqi_ext.parse_program_term x `pb
                      |> program_of_piqi 
(* string -> intent option *)
let intent_of_pb x = Some (Ir_piqi_ext.parse_intent x `pb) |> intent_of_piqi
(*  string -> typeid *)
let typeid_of_pb x= Ir_piqi_ext.parse_typeid x `pb |> typeid_of_piqi
(* string -> Blk.t *)
let blk_of_pb x= Ir_piqi_ext.parse_blk_term x `pb |> blk_of_piqi
(* string -> Arg.t *)
let arg_of_pb x=Ir_piqi_ext.parse_arg_term x `pb |> arg_of_piqi 
(* string -> Sub.t *)
let sub_of_pb x =Ir_piqi_ext.parse_sub_term x `pb |> sub_of_piqi
(* string -> Phi.t *)
let phi_of_pb x=Ir_piqi_ext.parse_phi_term x `pb |> phi_of_piqi
(* string -> Jmp.t *)
let jmp_of_pb x=Ir_piqi_ext.parse_jmp_term x `pb |> jmp_of_piqi
(* string -> Def.t *)
let def_of_pb x=Ir_piqi_ext.parse_def_term x `pb |> def_of_piqi
(* string -> call *)
let call_of_pb x=Ir_piqi_ext.parse_call x `pb |> call_of_piqi
(* string -> label *)
let label_of_pb x=Ir_piqi_ext.parse_label x `pb |> label_of_piqi
(* string -> jmp_kind *)
let jmp_kind_of_pb x=Ir_piqi_ext.parse_jmp_kind x `pb |> jmp_kind_of_piqi
