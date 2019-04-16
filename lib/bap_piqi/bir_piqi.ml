open Core_kernel
open Bap.Std

open Option.Monad_infix

module R = Ir_piqi


module Get = struct
  let tid = Tid.from_string_exn
  let exp = Bil_piqi.exp_of_piqi
  let var = Bil_piqi.var_of_piqi

  let label = function
    | `direct t -> Direct (tid t)
    | `indirect e -> Indirect (exp e)

  let intent = function
    | `in_ -> In
    | `out -> Out
    | `both -> Both

  let jump = function
    | `goto dst -> Goto (label dst)
    | `ret dst -> Ret (label dst)
    | `exn  {R.Exn.number; return=r} -> Int (number, tid r)
    | `call {R.Call.target=t; return=r} ->
      Call
        (Call.create ?return:(r >>| label) ~target:(label t) ())

  let arg {R.Arg.tid=t; lhs=v; rhs=e; intent=i} =
    Arg.create ~tid:(tid t) ?intent:(i >>| intent) (var v) (exp e)

  let def {R.Def.tid=t; lhs; rhs} =
    Def.create ~tid:(tid t) (var lhs) (exp rhs)

  let phi {R.Phi.tid=t; lhs; values} =
    values |>
    List.map ~f:(fun {R.Tid_exp_pair.tid=t;exp=e} -> tid t, exp e) |>
    Phi.of_list ~tid:(tid t) (var lhs)

  let jmp {R.Jmp.tid=t; cond=c; jump=j} =
    Jmp.create ~tid:(tid t) ~cond:(exp c) (jump j)

  let blk {R.Blk.tid=t; phis; defs; jmps} =
    let b = Blk.Builder.create ~tid:(tid t) () in
    List.iter phis ~f:(fun p -> Blk.Builder.add_phi b (phi p));
    List.iter defs ~f:(fun p -> Blk.Builder.add_def b (def p));
    List.iter jmps ~f:(fun p -> Blk.Builder.add_jmp b (jmp p));
    Blk.Builder.result b

  let sub {R.Sub.tid=t; args; blks; name} =
    let b = Sub.Builder.create ~tid:(tid t) ~name () in
    List.iter args ~f:(fun x -> Sub.Builder.add_arg b (arg x));
    List.iter blks ~f:(fun x -> Sub.Builder.add_blk b (blk x));
    Sub.Builder.result b

  let program {R.Program.tid=t; subs} =
    let b = Program.Builder.create ~tid:(tid t) () in
    List.iter subs ~f:(fun x -> Program.Builder.add_sub b (sub x));
    Program.Builder.result b
end

module Put = struct
  let tid = Tid.to_string
  let exp = Bil_piqi.piqi_of_exp
  let var = Bil_piqi.piqi_of_var

  let label = function
    | Direct t -> `direct (tid t)
    | Indirect e -> `indirect (exp e)

  let intent = function
    | In -> `in_
    | Out -> `out
    | Both -> `both

  let jump = function
    | Goto d -> `goto (label d)
    | Ret d -> `ret (label d)
    | Int (n,r) -> `exn R.Exn.{number=n; return=(tid r)}
    | Call c -> `call R.Call.{
        target = label (Call.target c);
        return = Call.return c >>| label;
      }

  let arg a = R.Arg.{
      tid = tid (Term.tid a);
      lhs = var (Arg.lhs a);
      rhs = exp (Arg.rhs a);
      intent = Arg.intent a >>| intent;
    }

  let def d = R.Def.{
      tid = tid (Term.tid d);
      lhs = var (Def.lhs d);
      rhs = exp (Def.rhs d);
    }

  let pair (t,e) = R.Tid_exp_pair.{tid = tid t; exp = exp e}

  let phi t = R.Phi.{
      tid = tid (Term.tid t);
      lhs = var (Phi.lhs t);
      values = Phi.values t |> Seq.map ~f:pair |> Seq.to_list
    }

  let jmp t = R.Jmp.{
      tid = tid (Term.tid t);
      cond = exp (Jmp.cond t);
      jump = jump (Jmp.kind t);
    }

  let list t f term =
    Term.enum t term |> Seq.map ~f |> Seq.to_list

  let blk t = R.Blk.{
      tid = tid (Term.tid t);
      phis = list phi_t phi t;
      defs = list def_t def t;
      jmps = list jmp_t jmp t;
    }

  let sub t = R.Sub.{
      tid = tid (Term.tid t);
      name = Sub.name t;
      args = list arg_t arg t;
      blks = list blk_t blk t;
    }

  let program t = R.Program.{
      tid = tid (Term.tid t);
      subs = list sub_t sub t;
    }
end


type fmt = [ `json | `pb | `piq | `pib | `xml ]
type out_fmt = [fmt | `json_pretty | `xml_pretty]

let loads f g fmt s = f (g s fmt)
let dumps g f fmt x = f (g x) (fmt : fmt :> out_fmt)

open Ir_piqi_ext

let program_of_string = loads Get.program parse_program
let sub_of_string     = loads Get.sub parse_sub
let blk_of_string     = loads Get.blk parse_blk
let arg_of_string     = loads Get.arg parse_arg
let phi_of_string     = loads Get.phi parse_phi
let def_of_string     = loads Get.def parse_def
let tid_of_string     = loads Get.tid parse_tid

let string_of_program = dumps Put.program gen_program
let string_of_sub     = dumps Put.sub gen_sub
let string_of_blk     = dumps Put.blk gen_blk
let string_of_arg     = dumps Put.arg gen_arg
let string_of_phi     = dumps Put.phi gen_phi
let string_of_def     = dumps Put.def gen_def
let string_of_tid     = dumps Put.tid gen_tid
