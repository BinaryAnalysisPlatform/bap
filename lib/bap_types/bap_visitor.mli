(** AST Visitors.

    This module provides three classes that visits AST:

    [visitor] that folds arbitrary value over the AST,
    [finder] is a visitor, that can prematurely finish the traversal,
    [mapper] that maps AST, allowing limited transformation
    of its structure.

    You can find some handy transformations in `Bap_helpers`
    module. Note, all definitions from this module and `Bap_helpers`
    is available under `Bil` namespace.


*)
open Core_kernel.Std
open Bap_common
open Bap_bil

(** Both visitors provides some information about the current
    position of the visitor *)
class state : object
  (** the stack of stmts that was already visited, with the last on
      the top. Not including the currently visiting stmt. *)
  val preds : bil

  (** stmts that are not yet visited  *)
  val succs : bil

  (** a stack of stmts that are parents for the currently visiting
      entity. The top one is the one that we're currently visiting. *)
  val stmts_stack : bil

  (** a stack of expr, that are parents for the currenly visiting
      expression *)
  val exps_stack  : exp  list

  (** is [true] if we're visiting expression that is a jump target *)
  val in_jmp : bool

  (** is [true] if we're visiting expression that is on the left or
      right side of the assignment. *)
  val in_move : bool

  (** is [true] if currently visiting expression or statement is
      executed under condition.  *)
  val under_condition : bool
  (** is [true] if currently visiting expression or statement is
      executed under loop.  *)
  val in_loop : bool
end

(** Visitor.
    Visits AST providing lots of hooks.

    For each AST constructor [C] the visitor provides three methods:
    [enter_C], [visit_C], [leave_C]. The default implementation for
    [enter_C] and [leave_C] is to return its argument. The default
    implementation for [visit_C] is the following:
    1. call [enter_C]
    2. visit all children
    3. call [leave_C].

    It is recommended to override [enter_C] method if you only need
    to visit [C] constructor without changing a way you're visiting
    the tree.

    For example, to collect all resolved jumps one could write the
    following function:

    {[
      let collect_calls bil = (object(self)
        inherit [Word.t list] visitor
        method! enter_int x js = if in_jmp then x :: js else js
      end)#run bil []
    ]}

    The default entry point of the visitor is method [run], but
    you can use any other method as well, for example, if you do
    not have a statement at all and want to visit expression.
*)
class ['a] visitor : object
  inherit state
  (** {3 Default entry point}  *)
  method run : bil -> 'a -> 'a

  (** {3 Statements }  *)
  method enter_stmt : stmt -> 'a -> 'a
  method visit_stmt : stmt -> 'a -> 'a
  method leave_stmt : stmt -> 'a -> 'a

  (** {4 [Move(var,exp)]}  *)
  method enter_move : var -> exp -> 'a -> 'a
  method visit_move : var -> exp -> 'a -> 'a
  method leave_move : var -> exp -> 'a -> 'a

  (** {4 [Jmp exp]}  *)
  method enter_jmp : exp -> 'a -> 'a
  method visit_jmp : exp -> 'a -> 'a
  method leave_jmp : exp -> 'a -> 'a

  (** {4 [While (cond,bil)]}  *)
  method enter_while : cond:exp -> bil -> 'a -> 'a
  method visit_while : cond:exp -> bil -> 'a -> 'a
  method leave_while : cond:exp -> bil -> 'a -> 'a

  (** {4 [If (cond,yes,no)]}  *)
  method enter_if : cond:exp -> yes:bil -> no:bil -> 'a -> 'a
  method visit_if : cond:exp -> yes:bil -> no:bil -> 'a -> 'a
  method leave_if : cond:exp -> yes:bil -> no:bil -> 'a -> 'a

  (** {4 [CpuExn n]}  *)
  method enter_cpuexn : int -> 'a -> 'a
  method visit_cpuexn : int -> 'a -> 'a
  method leave_cpuexn : int -> 'a -> 'a

  (** {4 [Special string]}  *)
  method enter_special : string -> 'a -> 'a
  method visit_special : string -> 'a -> 'a
  method leave_special : string -> 'a -> 'a

  (** {3 Expressions}  *)
  method enter_exp : exp -> 'a -> 'a
  method visit_exp : exp -> 'a -> 'a
  method leave_exp : exp -> 'a -> 'a

  (** {4 [Load (src,addr,endian,size)]}  *)
  method enter_load : src:exp -> addr:exp -> endian -> size -> 'a -> 'a
  method visit_load : src:exp -> addr:exp -> endian -> size -> 'a -> 'a
  method leave_load : src:exp -> addr:exp -> endian -> size -> 'a -> 'a

  (** {4 [Store (dst,addr,src,endian,size)]}  *)
  method enter_store : dst:exp -> addr:exp -> src:exp -> endian -> size -> 'a -> 'a
  method visit_store : dst:exp -> addr:exp -> src:exp -> endian -> size -> 'a -> 'a
  method leave_store : dst:exp -> addr:exp -> src:exp -> endian -> size -> 'a -> 'a

  (** {4 [BinOp (op,e1,e2)]}  *)
  method enter_binop : binop -> exp -> exp -> 'a -> 'a
  method visit_binop : binop -> exp -> exp -> 'a -> 'a
  method leave_binop : binop -> exp -> exp -> 'a -> 'a

  (** {4 [Unop (op,e)]}  *)
  method enter_unop : unop -> exp -> 'a -> 'a
  method visit_unop : unop -> exp -> 'a -> 'a
  method leave_unop : unop -> exp -> 'a -> 'a

  (** {4 [Cast(kind,size,e)]}  *)
  method enter_cast : cast -> nat1 -> exp -> 'a -> 'a
  method visit_cast : cast -> nat1 -> exp -> 'a -> 'a
  method leave_cast : cast -> nat1 -> exp -> 'a -> 'a

  (** {4 [Let (v,exp,body)]}  *)
  method enter_let : var -> exp:exp -> body:exp -> 'a -> 'a
  method visit_let : var -> exp:exp -> body:exp -> 'a -> 'a
  method leave_let : var -> exp:exp -> body:exp -> 'a -> 'a

  (** {4 [Ite (cond,yes,no)]}  *)
  method enter_ite : cond:exp -> yes:exp -> no:exp -> 'a -> 'a
  method visit_ite : cond:exp -> yes:exp -> no:exp -> 'a -> 'a
  method leave_ite : cond:exp -> yes:exp -> no:exp -> 'a -> 'a

  (** {4 [Extract (hi,lo,e)]}  *)
  method enter_extract : hi:nat1 -> lo:nat1 -> exp -> 'a -> 'a
  method visit_extract : hi:nat1 -> lo:nat1 -> exp -> 'a -> 'a
  method leave_extract : hi:nat1 -> lo:nat1 -> exp -> 'a -> 'a

  (** {4 [Concat(e1,e2)]}  *)
  method enter_concat : exp -> exp -> 'a -> 'a
  method visit_concat : exp -> exp -> 'a -> 'a
  method leave_concat : exp -> exp -> 'a -> 'a

  (** {3 [Leafs]} *)
  (** {4 [Int w]}  *)
  method enter_int : word -> 'a -> 'a
  method visit_int : word -> 'a -> 'a
  method leave_int : word -> 'a -> 'a

  (** {4 [Var v]}  *)
  method enter_var : var -> 'a -> 'a
  method visit_var : var -> 'a -> 'a
  method leave_var : var -> 'a -> 'a

  (** {4 [Unknown (str,typ)]}  *)
  method enter_unknown : string -> typ -> 'a -> 'a
  method visit_unknown : string -> typ -> 'a -> 'a
  method leave_unknown : string -> typ -> 'a -> 'a
end


(** A visitor with shortcut.
    Finder is a specialization of a visitor, that uses [return] as its
    folding argument. At any time you can stop the traversing by
    calling [return] function of the provided argument (which is by
    itself is a record with one field - a function accepting argument
    of type ['a option]).

    For example, the following function will check whether [x]
    variable is referenced in the provided scope.
    {[
      let is_referenced x = find (object(self)
          inherit [unit] finder
          method! enter_var y cc =
            if Bap_var.(x = y) then cc.return (Some ()); cc
        end)
    ]}

    Note: the example uses [find] function from the [Bap_helpers].
*)
class ['a] finder : object
  inherit ['a option return] visitor
  method find : bil -> 'a option
end

(** AST transformation.
    mapper allows one to map AST, performing some limited
    amount of transformations on it. Mapper provides extra
    flexibility by mapping [stmt] to [stmt list], thus allowing
    to remove statements from the output (by mapping to empty list) or
    to map one statement to several. This is particularly useful when
    you map [if] or [while] statements.
*)
class mapper : object
  inherit state

  (** Default entry point.
      But again, you can use any method as an entry  *)
  method run : bil -> bil

  (** {3 Statements}  *)
  method map_stmt : stmt -> bil
  method map_move : var -> exp -> bil
  method map_jmp : exp -> bil
  method map_while : cond:exp -> bil -> bil
  method map_if : cond:exp -> yes:bil -> no:bil -> bil
  method map_cpuexn : int -> bil
  method map_special : string -> bil

  (** {3 Expressions}  *)
  method map_exp : exp -> exp
  method map_load : src:exp -> addr:exp -> endian -> size -> exp
  method map_store : dst:exp -> addr:exp -> src:exp -> endian -> size -> exp
  method map_binop : binop -> exp -> exp -> exp
  method map_unop : unop -> exp -> exp
  method map_cast : cast -> nat1 -> exp -> exp
  method map_let : var -> exp:exp -> body:exp -> exp
  method map_ite : cond:exp -> yes:exp -> no:exp -> exp
  method map_extract : hi:nat1 -> lo:nat1 -> exp -> exp
  method map_concat : exp -> exp -> exp
  method map_int : word -> exp
  method map_var : var -> exp
  method map_sym : var -> var
  method map_unknown : string -> typ -> exp
end
