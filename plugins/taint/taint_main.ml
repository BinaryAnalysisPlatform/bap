open Core_kernel.Std
open Bap.Std
include Self()
open Format

type strain =
  | Addr of int64
  | Tid of string
  | Var of string
  [@@deriving variants]

let grammar = {|
    strain ::= <tid>
             | <var>
             | <addr>
             | (<strain1> <strain2> .. <strainK>)
    tid    ::= %<hex-digits>
    var    ::= <alpha> .. <alpha-num>
    addr   ::= 0x<hex-digits>
|}

module Strain = struct
  open Sexp.O
  open Result.Monad_infix

  exception Parse_error of string

  let expect exp ~got = Error (sprintf "expected %s got %S" exp got)
  let is_pref p = String.is_prefix ~prefix:p
  let is_addr x = is_pref "0x" x
  let is_term x = is_pref "%" x
  let is_var  x = try Char.is_alpha x.[0] with exn -> false
  let word s =
    try Ok (Int64.of_string s)
    with exn -> expect "0x<hex-digits>" ~got:s

  let var  s = Ok (Var s)
  let term s = Ok (Tid s)
  let addr s = word s >>| addr
  let atom = function
    | Atom s when is_addr s -> addr s
    | Atom s when is_term s -> term s
    | Atom s when is_var s  -> var s
    | s -> expect "<tid> | <var> | <addr>"
             ~got:(Sexp.to_string s)

  let sexp = function
    | List x -> List.map ~f:atom x
    | x -> [atom x]

  let start s = Result.all (sexp s)


  let parse s =
    try (start (Sexp.of_string s))
    with exn -> expect grammar ~got:s

  let to_string = function
    | Addr a -> sprintf "%0Lx" a
    | Tid s | Var s -> s

  let parser s = match parse s with
    | Ok r -> `Ok r
    | Error e -> `Error e

  let pp ppf ss =
    List.map ~f:to_string ss |>
    String.concat ~sep:" " |>
    Format.fprintf ppf "(%s)"

  let t = parser,pp,invalid_arg "Undefined default"
end


module Marker = struct
  let sats strains def =
    let tid = Term.tid def in
    List.for_all strains ~f:(function
        | Tid name -> Tid.name tid = name
        | Var name -> Var.name (Def.lhs def) = name
        | Addr a -> match Term.get_attr def Disasm.insn_addr with
          | None -> false
          | Some addr -> match Addr.to_int64 addr with
            | Error _ -> false
            | Ok x -> Int64.equal a x)

  let seed t = match Term.get_attr t Term.origin with
    | None -> Term.tid t
    | Some t -> t

  let mark taint strains def =
    let mark def = Term.set_attr def taint (seed def) in
    List.fold strains ~init:def ~f:(fun def strains ->
        if sats strains def then mark def else def)

  class main ~regs ~ptrs = object(self)
    inherit Term.mapper as super
    method map_def def =
      super#map_def def |>
      mark Taint.reg regs |>
      mark Taint.ptr ptrs
  end

  let run ~regs ~ptrs = (new main ~regs ~ptrs)#run
end


let mark regs ptrs proj =
  Project.program proj |>
  Marker.run ~regs ~ptrs |>
  Project.with_program proj

let main regs ptrs = match regs,ptrs with
  | [],[] -> ()
  | regs,ptrs ->
    Project.register_pass ~deps:["callsites"] ~autorun:true
      (mark regs ptrs)

module Cmdline = struct

  let man = [
    `S "DESCRIPTION";

    `P {|
Injects taints into a program based on a specification. It is possible
to taint a value stored in a register, or a value pointed by a value
stored in a register. The former is called a "register taint", the
latter is called a "pointer taint". They're controlled, respectively,
with $(b,--taint-reg=)$(i,STRAIN) and $(b,--taint-ptr=)$(i,STRAIN)
command line arguments.  The $(i,STRAIN) value describes what
definitions should be tainted. It can be either an address, a
variable, a tid or a list of strains. If an address is passed then a
definition is tainted if it corresponds to an instruction with the
specified address. If a variable is passed, the the definition is
tainted if it defines a variable with the given name. Finally, if tid
is specified, then a definition must have the specified tid to be
tainted. If several strains are specified, then all conditions must be
satisfied. Consider ther following examples, |};
    `Pre {|
     --taint-reg=0xBAD
     --taint-ptr=strcpy_dst
     --taint-reg="(0xBAD malloc_return)"
|};
    `P {|
The first example will taint a value stored in a register
defined by an instruction at address $(i,0xBAD). The second
example will taint a value that is pointed by a variable
$(i,strcpy_dst) that is defined after each call to a $(i,strcpy). (Note:
this functionality relies on API plugin, that is responsible for
embedding this definitions at the call sites). The third example will
taint values returned by a $(i,malloc) only at the specified call site
address. You can specify these options several times.|};
    `P "The full grammar specification of strain specification
language follows:";
    `Pre grammar
  ]

  let taints kind : strain list list Config.param =
    let doc = sprintf "Taint %s value of definition matching
     with the specification" kind in
    Config.(param_all Strain.t kind ~doc)

  let () =
    let reg = taints "reg" in
    let ptr = taints "ptr" in
    Config.manpage man;
    Config.when_ready (fun {Config.get=(!)} -> main !reg !ptr)

end
