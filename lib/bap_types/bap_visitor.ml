open Core_kernel.Std
open Bap_common
open Bap_bil


type nat1 = int

class state = object
  val stmts_stack : stmt list = []
  val exps_stack : exp list = []
  val preds : stmt list = []
  val succs : stmt list = []
  val in_jmp = false
  val in_move = false
  val under_condition = false
  val in_loop = false
end

class ['a] visitor = object (self : 's)
  inherit state
  method run stmts x : 'a = match stmts with
    | [] -> x
    | s :: ss ->
      {< succs = ss >}#visit_stmt s x |>
      {< preds = s :: preds >}#run ss

  method enter_unop op _ x = x
  method leave_unop op _ x = x
  method visit_unop op e x =
    self#enter_unop op e x |>
    self#visit_exp e |>
    self#leave_unop op e

  method enter_binop op e1 e2 x = x
  method leave_binop op e1 e2 x = x
  method visit_binop op e1 e2 x =
    self#enter_binop op e1 e2 x |>
    self#visit_exp e1 |> self#visit_exp e2 |>
    self#leave_binop op e1 e2

  method enter_store ~dst ~addr ~src e s x = x
  method leave_store ~dst ~addr ~src e s x = x
  method visit_store ~dst ~addr ~src e s x =
    self#enter_store ~dst ~addr ~src e s x |>
    self#visit_exp dst |> self#visit_exp addr |> self#visit_exp src |>
    self#leave_store ~dst ~addr ~src e s

  method enter_load ~src ~addr _e _s x = x
  method leave_load ~src ~addr _e _s x = x
  method visit_load ~src ~addr _e _s x =
    self#enter_load ~src ~addr _e _s x |>
    self#visit_exp src |> self#visit_exp addr |>
    self#leave_load ~src ~addr _e _s

  method enter_cast _ _ e x =  x
  method leave_cast _ _ e x =  x
  method visit_cast ct cs e x =
    self#enter_cast ct cs e x |> self#visit_exp e |>
    self#leave_cast ct cs e

  method enter_let v ~exp ~body x = x
  method leave_let v ~exp ~body x = x
  method visit_let v ~exp ~body x =
    self#enter_let v ~exp ~body x |>
    self#visit_var v |>
    self#visit_exp exp |> self#visit_exp body |>
    self#leave_let v ~exp ~body

  method enter_ite ~cond ~yes ~no x = x
  method leave_ite ~cond ~yes ~no x = x
  method visit_ite ~cond ~yes ~no x =
    let x = self#enter_ite ~cond ~yes ~no x |>
            self#visit_exp cond in
    let self = {< under_condition = true >} in
    self#visit_exp yes x |> self#visit_exp no |>
    self#leave_ite ~cond ~yes ~no

  method enter_extract ~hi ~lo e x = x
  method leave_extract ~hi ~lo e x = x
  method visit_extract ~hi ~lo e x =
    self#enter_extract ~hi ~lo e x |>
    self#visit_exp e |>
    self#leave_extract ~hi ~lo e

  method enter_concat e1 e2 x = x
  method leave_concat e1 e2 x = x
  method visit_concat e1 e2 x =
    self#enter_concat e1 e2 x |>
    self#visit_exp e1 |> self#visit_exp e2 |>
    self#leave_concat e1 e2

  method enter_var v x = x
  method leave_var v x = x
  method visit_var v x = self#enter_var v x |> self#leave_var v

  method enter_int n x = x
  method leave_int n x = x
  method visit_int n x =
    self#enter_int n x |> self#leave_int n

  method enter_unknown s t x = x
  method leave_unknown s t x = x
  method visit_unknown s t x =
    self#enter_unknown s t x |> self#leave_unknown s t

  method enter_special s x = x
  method leave_special s x = x
  method visit_special s x =
    self#enter_special s x |> self#leave_special s

  method enter_jmp e x = x
  method leave_jmp e x = x
  method visit_jmp e x =
    self#enter_jmp e x |>
    {< in_jmp = true >}#visit_exp e |>
    self#leave_jmp e

  method enter_move v e x = x
  method leave_move v e x = x
  method visit_move v e x =
    let x = self#enter_move v e x in
    let self = {< in_move = true >} in
    self#visit_var v x |> self#visit_exp e |>
    self#leave_move v e

  method enter_while ~cond ss x = x
  method leave_while ~cond ss x = x
  method visit_while ~cond ss x =
    let x = self#enter_while ~cond ss x |>
            self#visit_exp cond in
    let self = {< under_condition = true;
                  in_loop = true >} in
    self#run ss x |>
    self#leave_while ~cond ss

  method enter_if ~cond ~yes ~no x = x
  method leave_if ~cond ~yes ~no x = x
  method visit_if ~cond ~yes ~no x =
    let x = self#enter_if ~cond ~yes ~no x |>
            self#visit_exp cond in
    let self = {< under_condition = true >} in
    self#run yes x |> self#run no |>
    self#leave_if ~cond ~yes ~no

  method enter_cpuexn n x = x
  method leave_cpuexn n x = x
  method visit_cpuexn n x =
    self#enter_cpuexn n x |> self#leave_cpuexn n

  method enter_exp e x = x
  method leave_exp e x = x
  method visit_exp e x : 'a =
    let x = self#enter_exp e x in
    let self = {< exps_stack = e :: exps_stack >} in
    let x = match e with
      | Exp.Int v -> self#visit_int v x
      | Exp.UnOp (op,e) -> self#visit_unop op e x
      | Exp.BinOp (op,e1,e2) -> self#visit_binop op e1 e2 x
      | Exp.Store (dst,addr,src,e,s) -> self#visit_store ~dst ~addr ~src e s x
      | Exp.Load (src,addr,e,s) -> self#visit_load ~src ~addr e s x
      | Exp.Cast (ct,sz,ex) -> self#visit_cast ct sz ex x
      | Exp.Let (v,exp,body) -> self#visit_let v ~exp ~body x
      | Exp.Ite (cond,yes,no) -> self#visit_ite ~cond ~yes ~no x
      | Exp.Extract (hi,lo,e) -> self#visit_extract ~hi ~lo e x
      | Exp.Concat (e1,e2) -> self#visit_concat e1 e2 x
      | Exp.Var v -> self#visit_var v x
      | Exp.Unknown (s,t) -> self#visit_unknown s t x in
    self#leave_exp e x

  method enter_stmt stmt x = x
  method leave_stmt stmt x = x
  method visit_stmt stmt x =
    let x = self#enter_stmt stmt x in
    let self = {< stmts_stack = stmt :: stmts_stack >} in
    let x = match stmt with
      | Stmt.Move (var,exp) -> self#visit_move var exp x
      | Stmt.Jmp exp -> self#visit_jmp exp x
      | Stmt.Special s -> self#visit_special s x
      | Stmt.While (cond,ss) -> self#visit_while ~cond ss x
      | Stmt.If (cond,yes,no) -> self#visit_if ~cond ~yes ~no x
      | Stmt.CpuExn n -> self#visit_cpuexn n x in
    self#leave_stmt stmt x
end

exception Found

class ['a] finder = object(self)
  inherit ['a option return] visitor
  method find stmts : 'a option =
    with_return (fun cc ->
        ignore (self#run stmts cc);
        None)
end

class mapper = object (self : 's)
  inherit state
  method run stmts : stmt list =
    let rec loop self acc = function
      | [] -> List.rev acc
      | s :: ss ->
        let self = {< succs = ss>} in
        let acc = self#map_stmt s :: acc in
        let self = {< preds = s :: preds >} in
        loop self acc ss in
    loop self [] stmts |> List.concat

  method map_unop op e =
    Exp.UnOp (op, self#map_exp e)

  method map_binop op e1 e2=
    Exp.BinOp (op, self#map_exp e1, self#map_exp e2)

  method map_store ~dst ~addr ~src e s =
    Exp.Store (self#map_exp dst,
               self#map_exp addr,
               self#map_exp src, e, s)

  method map_load ~src ~addr e s =
    Exp.Load (self#map_exp src, self#map_exp addr, e, s)

  method map_cast ct cs e = Exp.Cast (ct,cs,self#map_exp e)

  method map_let v ~exp ~body =
    Exp.Let (self#map_sym v, self#map_exp exp, self#map_exp body)

  method map_ite ~cond ~yes ~no =
    let s = {< under_condition = true >} in
    Exp.Ite (self#map_exp cond, s#map_exp yes, s#map_exp no)

  method map_extract ~hi ~lo e =
    Exp.Extract (hi, lo, self#map_exp e)

  method map_concat e1 e2 =
    Exp.Concat (self#map_exp e1, self#map_exp e2)

  method map_var s = Exp.Var (self#map_sym s)

  method map_sym = Fn.id

  method map_int n = Exp.Int n

  method map_unknown s t = Exp.Unknown (s,t)

  method map_special s = [Stmt.Special s]

  method map_jmp e =
    [Stmt.Jmp ({< in_jmp = true >}#map_exp e)]

  method map_move v e =
    let self = {< in_move = true >} in
    [Stmt.Move (self#map_sym v, self#map_exp e)]

  method map_while ~cond ss =
    [Stmt.While (self#map_exp cond,
                 {< in_loop = true; under_condition = true >}#run ss)]

  method map_if ~cond ~yes ~no =
    let s = {< under_condition = true >} in
    [Stmt.If (self#map_exp cond, s#run yes, s#run no)]

  method map_cpuexn n = [Stmt.CpuExn n]

  method map_exp e : exp =
    let self = {< exps_stack = e :: exps_stack >} in
    match e with
    | Exp.Int v -> self#map_int v
    | Exp.UnOp (op,e) -> self#map_unop op e
    | Exp.BinOp (op,e1,e2) -> self#map_binop op e1 e2
    | Exp.Store (dst,addr,src,e,s) -> self#map_store ~dst ~addr ~src e s
    | Exp.Load (src,addr,e,s) -> self#map_load ~src ~addr e s
    | Exp.Cast (ct,sz,ex) -> self#map_cast ct sz ex
    | Exp.Let (v,exp,body) -> self#map_let v ~exp ~body
    | Exp.Ite (cond,yes,no) -> self#map_ite ~cond ~yes ~no
    | Exp.Extract (hi,lo,e) -> self#map_extract ~hi ~lo e
    | Exp.Concat (e1,e2) -> self#map_concat e1 e2
    | Exp.Var v -> self#map_var v
    | Exp.Unknown (s,t) -> self#map_unknown s t

  method map_stmt stmt : bil =
    let self = {< stmts_stack = stmt :: stmts_stack >} in
    match stmt with
    | Stmt.Move (var,exp) -> self#map_move var exp
    | Stmt.Jmp exp -> self#map_jmp exp
    | Stmt.Special s -> self#map_special s
    | Stmt.While (cond,ss) -> self#map_while ~cond ss
    | Stmt.If (cond,yes,no) -> self#map_if ~cond ~yes ~no
    | Stmt.CpuExn n -> self#map_cpuexn n
end
