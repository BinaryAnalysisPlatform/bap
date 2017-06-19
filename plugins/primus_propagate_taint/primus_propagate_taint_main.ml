open Core_kernel.Std
open Bap.Std
open Bap_primus.Std
open Monads.Std
open Format
include Self()

type taint_value = {
  ptr : Taint.set; 
  imm : Taint.set;
  value : word;
}

type taint_stack = taint_value list

type state = {
  tas : Taint.set Addr.Map.t;
  tvs : Taint.set Var.Map.t;
  terms : Taint.set Tid.Map.t;
  taints : taint_stack;
}

let state = Primus.Machine.State.declare
    ~name:"primus-taint"
    ~uuid:"2d4a4208-f918-4cf7-8e1b-5d8400a106d3"
    (fun _ -> {
         tas = Addr.Map.empty;
         tvs = Var.Map.empty;
         terms = Tid.Map.empty;
         taints = [];
       })


let pp_taint_value ppf {ptr;imm;value} = 
  fprintf ppf "{%s; imm=%a; ptr=%a}@\n" 
    (Word.string_of_value value) 
    Taint.pp_set imm
    Taint.pp_set ptr

let pp_taints ppf taints = 
  List.iter taints ~f:(pp_taint_value ppf)

let empty = Tid.Set.empty

let untainted x = {ptr = empty; imm = empty; value=x}

let taint taints key value = 
  Map.update taints key ~f:(function
      | None -> Tid.Set.singleton value
      | Some s -> Set.add s value)

module Main(Machine : Primus.Machine.S) = struct 
  open Machine.Syntax

  module Env = Primus.Env.Make(Machine)
  module Mem = Primus.Memory.Make(Machine)
  module Eval = Primus.Interpreter.Make(Machine)

  let introduce t add def = 
    match Term.get_attr def t with
    | None -> Machine.return () 
    | Some t -> 
      Machine.Local.get state >>=
      add def t >>= 
      Machine.Local.put state 

  let introduce_reg lhs = 
    introduce Taint.reg @@ fun def t s ->
    printf "introducing taint for %a@\n" 
      Var.pp (lhs def);
    Machine.return {
      s with tvs = taint s.tvs (lhs def) t 
    }

  let introduce_ptr lhs  =
    introduce Taint.ptr @@ fun def t s ->
    Env.get (lhs def) >>| fun addr -> {
      s with tas = taint s.tas addr t
    }

  let taints_of_var {tvs} v = 
    match Map.find tvs v with
    | None -> empty
    | Some ts -> 
      printf "var %s is tainted with %a@\n"
        (Var.name v) Taint.pp_set ts;
      ts

  (* folds over first [n] elements of a list [xs] *)
  let fold_n n xs ~init ~f = 
    let rec loop acc i = function
      | x :: xs when i < n -> loop (f acc x) (i+1) xs
      | _ -> acc in
    loop init 0 xs

  (* reduces the computation stack by poping the result of
     compuation, and folding it with n values beneath it *)
  let reduce n xs ~f = match xs with
    | [] -> failwith "computation stack is exhausted"
    | x :: xs -> fold_n n xs ~init:x ~f

  let union_taints t {ptr;imm} = {
    t with 
    ptr = Set.union t.ptr ptr;
    imm = Set.union t.imm imm;
  }

  (* pops [n] elements from the stack, merges their taints and pushes
     the result to the taint stack *) 
  let propagate n = 
    Machine.Local.update state ~f:(fun s ->
        reduce n s.taints ~f:union_taints |> fun ts -> {
          s with taints = ts :: List.drop s.taints (n+1)
        })

  let init_taint s x = match Map.find s.tas x with
    | None -> untainted x
    | Some ptr -> {ptr; imm=empty; value=x} 

  let val_computed x = 
    Machine.Local.update state ~f:(fun s -> 
        {s with taints = init_taint s x :: s.taints})

  let map_fst xs ~f = match xs with
    | [] -> []
    | x :: xs -> f x :: xs

  let var v = 
    Machine.Local.update state ~f:(fun s -> {
          s with
          taints = map_fst s.taints ~f:(fun t -> {
                t with 
                imm = Set.union t.imm (taints_of_var s v)
              })})

  let merge_load result addr = {
    result with
    imm = Set.union result.imm addr.ptr;
  }

  let merge_store s mem value addr taints = {
    s with 
    tas = Map.add s.tas ~key:addr.value ~data:value.imm;
    taints = mem :: taints
  }

  let load = 
    Machine.Local.update state ~f:(fun s -> 
        match s.taints with
        | result :: addr :: _mem :: rest -> {
            s with taints = merge_load result addr :: rest
          }
        | _ -> failwith "load: corrupted stack")

  let store = 
    Machine.Local.update state ~f:(fun s -> 
        match s.taints with
        | mem :: value :: addr :: _ :: rest -> 
          merge_store s mem value addr rest
        | _ -> failwith "store: corrupted stack")

  let ite =
    Machine.Local.update state ~f:(fun s -> {
          s with
          taints = match s.taints with
            | result :: _ :: _ :: rest -> result :: rest
            | _ -> failwith "ite: corrupted stack"
        })

  let exp_computed = function
    | Bil.Var v -> var v
    | Bil.Load _ -> load
    | Bil.Store _ -> store
    | Bil.Ite _ -> ite
    | Bil.BinOp _ | Bil.Concat _ -> propagate 2
    | Bil.UnOp _ | Bil.Cast _ | Bil.Extract _ -> propagate 1
    | Bil.Int _ | Bil.Unknown _ | Bil.Let _ -> Machine.return ()


  let merge_terms terms tid rhs = 
    Map.update terms tid ~f:(function
        | None -> Set.union rhs.imm rhs.ptr
        | Some ts -> Tid.Set.union_list [ts; rhs.imm; rhs.ptr])

  let propagate_var s tid lhs rhs = {
    s with
    tvs = Map.add s.tvs ~key:lhs ~data:rhs.imm;
    terms = merge_terms s.terms tid rhs
  }

  let def_computed lhs d = 
    Machine.Local.update state ~f:(fun s -> 
        match s.taints with
        | [value] -> propagate_var s (Term.tid d) (lhs d) value
        | [] -> s                (* for args *)
        | _ -> invalid_arg "def: corrupted stack")

  let def_computed lhs d = 
    def_computed lhs d >>= fun () -> 
    introduce_ptr lhs d >>= fun () ->
    introduce_reg lhs d

  let jmp_computed t = 
    Machine.Local.update state ~f:(fun s -> 
        match s.taints with
        | [value] -> {
            s with 
            terms = merge_terms s.terms (Term.tid t) value
          }
        | _ -> invalid_arg "jmp: corrupted stack")

  let pos_entered _ = 
    Machine.Local.update state ~f:(fun s -> {s with taints=[]})

  let finished () = 
    Machine.Local.get state >>| fun {terms} -> 
    Map.iteri terms ~f:(fun ~key:tid ~data:taints -> 
        printf "%a => %a@\n" Tid.pp tid Taint.pp_set taints)

  let debug_taints _ =  
    Machine.Local.get state >>| fun {taints} -> 
    printf "<taints>@\n%a@\n</taints>@\n" pp_taints taints

  let debug_tainted_vars _ =
    Machine.Local.get state >>| fun {tvs} -> 
    printf "<tvs>@\n%a@\n</tvs>" Taint.pp_map tvs

  let init () = Machine.List.all_ignore Primus.Interpreter.[
      new_value >>> val_computed;
      leave_exp >>> exp_computed;
      leave_def >>> def_computed Def.lhs;
      leave_arg >>> def_computed Arg.lhs;
      leave_jmp >>> jmp_computed;
      enter_pos >>> pos_entered;
      Primus.Machine.finished >>> finished;
      leave_exp >>> debug_taints;
    ]
  (* 
     after each expression we push a word onto the stack, once the
     expression is evaluated we pop the result, that is on the top
     of the stack, and the number of arguments of the expression,
     then we propagate the taint from the arguments to the result, 
     and push it back, example


     Store(m, x0, (Load(m,x1) + Load(m,x2)))

       m  -> {00; imm=[]; ptr=[]}
       x0 -> {DE; imm=[]; ptr=[1]}
           m  -> {00; imm=[]; ptr=[]}
           x1 -> {AD; imm=[2];ptr=[3]}
         Load(m,x1) -> {DE,imm=[3]; ptr=[]}
           m  -> {00; imm=[]; ptr=[]}
           x2 -> {AD; imm=[];ptr=[]}
         Load(m,x2) -> {ED;imm=[]; ptr=[4]}
       Load(m,x1) + Load(m,x2) -> {1CB; imm=[3]; ptr=[]}
     Store(m, x0, (Load(m,x1) + Load(m,x2))) -> {00; imm=[]; ptr=[]}

 *)
end

let enable () =
  info "Enabling taint propagation";
  Primus.Machine.add_component (module Main)

open Config;;
manpage [
  `S "DESCRIPTION";
  `P "The Primus taint propagatation engine.";
]

let enabled = 
  flag "run" ~doc:"Run taint propagation."

let () = when_ready (fun {get=(!!)} -> if !!enabled then enable ())
