open Core_kernel
open Bap_core_theory
open Bap_main
open KB.Syntax

let package = "core"
let herbrand_provides = [
  "syntax";
  "herbrand";
  "lisp";
  "debug";
  "core:eff";
  "core:val"
]

let decide_name_from_possible_name () : unit =
  KB.Rule.(declare ~package "name-of-possible-names" |>
           require Theory.Label.possible_name |>
           provide Theory.Label.name |>
           comment "resolves possible name");
  KB.promise Theory.Label.name @@
  KB.resolve Theory.Label.possible_name


let domain = KB.Domain.optional "cst"
    ~equal:Sexp.equal
    ~inspect:ident

let eslot = KB.Class.property Theory.Semantics.cls "eff"
    ~package:"core"
    ~public:true
    domain

let pslot = KB.Class.property Theory.Value.cls "val"
    ~package:"core"
    ~public:true
    domain

module Herbrand : Theory.Core = struct
  type t = Sexp.t


  let pure s cst = KB.Value.put pslot (Theory.Value.empty s) (Some cst)
  let eff s cst = KB.Value.put eslot (Theory.Effect.empty s) (Some cst)
  let data = eff Theory.Effect.Sort.bot
  let ctrl = eff Theory.Effect.Sort.bot
  let ret = KB.return
  let atom s = Sexp.Atom s

  let list = function
    | [Sexp.Atom op;
       List ((Atom opx) :: xs);
       List ((Atom opy) :: ys)]
      when String.(op = opx && op = opy) ->
      Sexp.List (Atom op :: List.append xs ys)
    | [Sexp.Atom op; List ((Atom opx) :: xs); y]
      when String.(op = opx) ->
      Sexp.List (Sexp.Atom op :: xs@[y])
    | [Sexp.Atom op; x; List ((Atom opy) :: ys)]
      when String.(op = opy) ->
      Sexp.List (Sexp.Atom op :: x :: ys)
    | xs -> Sexp.List xs

  let app x xs = list (atom x :: xs)

  let psort = Theory.Value.sort
  let esort = Theory.Effect.sort

  let (>>->) x f =
    x >>= fun v ->
    let s = psort v in
    f s (KB.Value.get pslot v)

  let (>>|>) x f = x >>-> fun s v -> ret (f s v)

  let (>>->?) x f =
    x >>= fun v ->
    let s = psort v in
    match KB.Value.get pslot v with
    | None -> ret (Theory.Value.empty s)
    | Some v -> f s v


  let (>>|>?) x f = x >>->? fun s v -> ret (f s v)

  let (>>=>) x f =
    x >>= fun v ->
    let s = esort v in
    f s (KB.Value.get eslot v)

  let (>>=>?) x f =
    x >>= fun v ->
    let s = esort v in
    match KB.Value.get eslot v with
    | None -> f s (list [])
    | Some x -> f s x


  let empty = Theory.Value.empty

  let unary_s s op x = x >>|> fun _ -> function
    | None -> empty s
    | Some v -> pure s @@ app op [v]

  let unary op x = x >>|>? fun s v -> pure s @@ app op [v]

  let monoid_s s op x y =
    x >>-> fun _ x ->
    y >>|> fun _ y ->
    match x, y with
    | Some x, Some y -> pure s @@ app op [x; y]
    | _ -> empty s

  let monoid op x y =
    x >>->? fun s x ->
    y >>|>? fun _ y ->
    pure s @@ app op [x; y]

  module Minimal = struct
    let b0 = ret@@pure Theory.Bool.t (atom "0")
    let b1 = ret@@pure Theory.Bool.t (atom "1")

    let unk s = ret@@empty s

    let var v =
      let s = Theory.Var.sort v in
      ret@@pure s@@atom (Theory.Var.name v)

    let let_ v x y =
      x >>-> fun _ x ->
      y >>|> fun s y ->
      let name = Theory.Var.name v in
      match x,y with
      | Some x, Some y ->
        pure s@@app "let" [atom name; x; y]
      | _ -> empty s

    let ite c x y =
      c >>-> fun _ c ->
      x >>->? fun s x ->
      y >>|>? fun _ y -> match c with
      | None -> empty s
      | Some c -> pure s@@app "ite" [c; x; y]

    let inv = unary "not"
    let and_ = monoid "logand"
    let or_ = monoid "logor"
    let int s x = ret@@pure s@@atom (Bitvec.to_string x)
    let msb x = unary_s Theory.Bool.t "msb" x
    let lsb x = unary_s Theory.Bool.t "lsb" x
    let neg x = unary "-" x
    let not x = unary "not" x
    let add x = monoid "+" x
    let sub x = monoid "-" x
    let mul x = monoid "*" x
    let div x = monoid "/" x
    let sdiv x = monoid "s/" x
    let modulo x = monoid "mod" x
    let smodulo x = monoid "signed-mod" x
    let logand x = monoid "logand" x
    let logor x = monoid "logor" x
    let logxor x = monoid "logxor" x

    let genshift name fill x off =
      fill >>-> fun _ fill ->
      x >>-> fun s x ->
      off >>|> fun _ off ->
      match fill, x, off with
      | Some fill, Some x, Some off ->
        pure s @@ app name [fill; x; off]
      | _ -> empty s
    let shiftr x = genshift "shiftr" x
    let shiftl x = genshift "shiftl" x
    let sle x = monoid_s Theory.Bool.t "s<=" x
    let ule x = monoid_s Theory.Bool.t "<" x


    let cast s fill exp =
      fill >>-> fun _ fill ->
      exp >>|> fun s' x ->
      let ct = sprintf "%d" @@ Theory.Bitv.size s in
      match fill, x  with
      | Some fill, Some x ->
        if Theory.Value.Sort.same s s'
        then pure s x
        else
          pure s@@list [
            atom "cast";
            atom ct;
            fill;
            x
          ]
      | _ -> empty s

    let concat s xs =
      List.map xs ~f:(fun x -> x >>|> fun _ -> ident) |>
      KB.List.all >>| Option.all >>| function
      | None -> empty s
      | Some xs -> pure s @@ app "concat" xs

    let append s x y = monoid_s s "append" x y

    let load m x =
      m >>-> fun s m ->
      x >>|> fun _ x ->
      let s = Theory.Mem.vals s in
      match m, x with
      | Some m, Some x -> pure s @@ app "load" [m; x]
      | _ -> empty s

    let store m p x =
      m >>-> fun s m ->
      p >>-> fun _ p ->
      x >>|> fun _ x ->
      match m, p, x with
      | Some m, Some p, Some x ->
        pure s @@ app "store" [m; p; x]
      | _ -> empty s

    let nil = Theory.Effect.empty Theory.Effect.Sort.bot

    let perform eff = ret (Theory.Effect.empty eff)

    let set v x = x >>|> fun _ x ->
      match x with
      | None -> nil
      | Some x -> data@@app "set" [
          atom (Theory.Var.name v);
          x
        ]

    let jmp x = x >>|> fun _ x -> match x with
      | None -> nil
      | Some x -> ctrl@@app "goto" [x]

    let goto dst =
      KB.collect Theory.Label.addr dst >>= function
      | Some dst ->
        ret@@ctrl@@app "goto" [atom (Bitvec.to_string dst)]
      | None ->
        KB.Object.repr Theory.Program.cls dst >>= fun dst ->
        ret@@ctrl@@app "goto" [atom dst]

    let both s xs ys =
      match xs,ys with
      | None,None -> ret nil
      | Some r,None
      | None, Some r -> ret@@eff s r
      | Some xs, Some ys ->
        ret@@eff s@@list [xs; ys]

    let seq xs ys =
      xs >>=> fun s xs ->
      ys >>=> fun _ ys ->
      both s xs ys

    let blk _ xs ys =
      xs >>=> fun _ xs ->
      ys >>=> fun _ ys ->
      both Theory.Effect.Sort.top xs ys

    let repeat cnd body =
      cnd >>-> fun _ cnd ->
      body >>=>? fun s body ->
      match cnd with
      | None -> ret@@nil
      | Some cnd ->
        ret@@eff s@@app "while" [cnd; body]

    let branch cnd yes nay =
      cnd >>-> fun _ cnd ->
      yes >>=>? fun s yes ->
      nay >>=>? fun _ nay ->
      match cnd with
      | None -> ret@@nil
      | Some cnd ->
        ret@@eff s@@app "if" [cnd; yes; nay]
  end
  include Theory.Basic.Make(Minimal)

  let mk_cast name s x =
    x >>|> fun s' x -> match x with
    | None -> empty s
    | Some x ->
      if Theory.Value.Sort.same s s'
      then pure s x
      else pure s@@app name [
          atom@@sprintf "%d" (Theory.Bitv.size s);
          x
        ]

  let high s = mk_cast "high" s
  let low s = mk_cast "low" s
  let signed s = mk_cast "signed" s
  let unsigned s = mk_cast "unsigned" s

  let extract s lo hi x =
    lo >>-> fun _ lo ->
    hi >>-> fun _ hi ->
    x >>|> fun _ x ->
    match lo,hi,x with
    | Some lo, Some hi, Some x ->
      pure s@@app "extract" [lo; hi; x]
    | _ -> empty s

  let loadw s dir mem ptr =
    dir >>-> fun _ dir ->
    mem >>-> fun _ mem ->
    ptr >>|> fun _ ptr ->
    match dir,mem,ptr with
    | Some dir, Some mem, Some ptr ->
      pure s@@app "loadw" [
        atom@@sprintf "%d" (Theory.Bitv.size s);
        dir;
        mem;
        ptr
      ]
    | _ -> empty s

  let storew dir mem ptr exp =
    dir >>-> fun _ dir ->
    mem >>-> fun s mem ->
    ptr >>-> fun _ ptr ->
    exp >>|> fun _ exp ->
    match Option.all [dir; mem; ptr; exp] with
    | Some args -> pure s@@app "storew" args
    | _ -> empty s

  let mk_shift name x m =
    x >>->? fun s x ->
    m >>|> fun _ -> function
    | None -> empty s
    | Some m -> pure s@@app name [x; m]

  let arshift x = mk_shift "arshift" x
  let rshift x = mk_shift "rshift" x
  let lshift x = mk_shift "lshift" x
  let eq x = monoid_s Theory.Bool.t "=" x
  let neq x = monoid_s Theory.Bool.t "/=" x
  let slt x = monoid_s Theory.Bool.t "s<" x
  let ult x = monoid_s Theory.Bool.t "<" x
  let sgt x = monoid_s Theory.Bool.t "s>" x
  let ugt x = monoid_s Theory.Bool.t ">" x
  let sge x = monoid_s Theory.Bool.t "s>=" x
  let uge x = monoid_s Theory.Bool.t ">=" x

  let asort s =
    atom@@Format.asprintf "%a" Theory.Value.Sort.pp (Theory.Value.Sort.forget s)


  let wellknown = Theory.IEEE754.[
      binary16,  ":b16";
      binary32,  ":b32";
      binary64,  ":b64";
      binary80,  ":b80";
      binary128, ":b128";
      decimal32, ":d32";
      decimal64, ":d64";
      decimal128, ":d128";
    ]

  let format s =
    let s = Theory.Value.Sort.forget s in
    match Theory.Float.(refine s) with
    | None -> asort s
    | Some s ->
      List.find_map wellknown ~f:(fun (par,name) ->
          let s' = Theory.IEEE754.Sort.define par in
          if Theory.Value.Sort.same s s'
          then Some (atom name)
          else None) |> function
      | Some s -> s
      | None -> asort s



  let float s x =
    x >>|> fun _ x -> match x with
    | None -> empty s
    | Some x -> pure s@@app "float" [format s; x]

  let fbits x =
    x >>|> fun s x ->
    let s = Theory.Float.bits s in
    match x with
    | None -> empty s
    | Some x -> pure s@@app "fbits" [x]

  let is_finite x = unary_s Theory.Bool.t "is-finite" x
  let is_nan x = unary_s Theory.Bool.t "is-nan" x
  let is_inf x = unary_s Theory.Bool.t "is-inf" x
  let is_fzero x = unary_s Theory.Bool.t "is-fzero" x
  let is_fpos x = unary_s Theory.Bool.t "is-fpos" x
  let is_fneg x = unary_s Theory.Bool.t "is-fneg" x

  let rmode s = ret@@pure Theory.Rmode.t @@ atom s
  let rne = rmode ":rne"
  let rna = rmode ":rna"
  let rtp = rmode ":rtp"
  let rtn = rmode ":rtn"
  let rtz = rmode ":rtz"
  let requal = eq

  let mk_fcast name s m x =
    m >>-> fun _ m ->
    x >>|> fun _ x ->
    match m,x with
    | Some m, Some x -> pure s@@app name [m;x]
    | _ -> empty s

  let cast_float s = mk_fcast "cast-float" s
  let cast_sfloat s = mk_fcast "cast-sfloat" s
  let cast_int s = mk_fcast "cast-int" s
  let cast_sint s = mk_fcast "cast-sint" s

  let fneg x = unary "fneg" x
  let fabs x = unary "fabs" x

  let monoid_f name m x y =
    x >>->? fun s x ->
    y >>->? fun _ y ->
    m >>|> fun _ m ->
    match m with
    | None -> empty s
    | Some m -> pure s@@app name [m; x; y]

  let unary_f name m x =
    x >>->? fun s x ->
    m >>|> fun _ m ->
    match m with
    | None -> empty s
    | Some m -> pure s@@app name [m; x]

  let ternary_f name m x y z =
    x >>->? fun s x ->
    y >>->? fun _ y ->
    z >>->? fun _ z ->
    m >>|> fun _ m ->
    match m with
    | None -> empty s
    | Some m -> pure s@@app name [m; x; y; z]

  let binary_fi name m f x =
    f >>->? fun s f ->
    m >>-> fun _ m ->
    x >>|> fun _ x ->
    match m,x with
    | Some m, Some x -> pure s@@app name [m; f; x]
    | _ -> empty s


  let fadd m = monoid_f "+." m
  let fsub m = monoid_f "-." m
  let fmul m = monoid_f "*." m
  let fdiv m = monoid_f "/." m
  let fmodulo m = monoid_f "mod." m
  let fsqrt m = unary_f "fsqrt" m
  let fround m = unary_f "fround" m
  let fmad m = ternary_f "fmad" m
  let fsucc x = unary "fsucc" x
  let fpred x = unary "fpred" x
  let forder x = monoid_s Theory.Bool.t "forder" x

  let fconvert s m x =
    m >>-> fun _ m ->
    x >>|> fun _ x ->
    match m,x with
    | Some m, Some x -> pure s@@app "fconvert" [
        format s; m; x
      ]
    | _ -> empty s

  let pow m = monoid_f "pow" m
  let hypot m = monoid_f "hypot" m

  let rsqrt m = unary_f "rsqrt" m
  let exp m = unary_f "exp" m
  let expm1 m = unary_f "expm1" m
  let exp2 m = unary_f "exp2" m
  let exp2m1 m = unary_f "exp2m1" m
  let exp10 m = unary_f "exp10" m
  let exp10m1 m = unary_f "exp10m1" m
  let log m = unary_f "log" m
  let log2 m = unary_f "log2" m
  let log10 m = unary_f "log10" m
  let logp1 m = unary_f "logp1" m
  let log2p1 m = unary_f "log2p1" m
  let log10p1 m = unary_f "log10p1" m
  let sin m = unary_f "sin" m
  let cos m = unary_f "cos" m
  let tan m = unary_f "tan" m
  let sinpi m = unary_f "sinpi" m
  let cospi m = unary_f "cospi" m
  let atanpi m = unary_f "atanpi" m
  let atan2pi m = monoid_f "atan2pi" m
  let asin m = unary_f "asin" m
  let acos m = unary_f "acos" m
  let atan m = unary_f "atan" m
  let atan2 m = monoid_f "atan2" m
  let sinh m = unary_f "sinh" m
  let cosh m = unary_f "cosh" m
  let tanh m = unary_f "tanh" m
  let asinh m = unary_f "asinh" m
  let acosh m = unary_f "acosh" m
  let atanh m = unary_f "atanh" m

  let compound m = binary_fi "compound" m
  let rootn m = binary_fi "rootn" m
  let pown m = binary_fi "pown" m
end


let herbrand_enabled = Extension.Configuration.parameter
    Extension.Type.(bool =? false) "syntax"
    ~as_flag:true
    ~aliases:["herbrand"; "debug"]

let enable_herbrand () =
  Theory.declare
    ~provides:herbrand_provides
    ~desc:"provides syntactic (herbrand) interpreation"
    ~package:"core"
    ~name:"syntax" (KB.return (module Herbrand : Theory.Core))

let () = Extension.declare @@ fun ctxt ->
  decide_name_from_possible_name ();
  if Extension.Configuration.get ctxt herbrand_enabled
  then enable_herbrand ();
  Ok ()
