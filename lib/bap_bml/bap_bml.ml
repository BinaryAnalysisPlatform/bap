open Core_kernel.Std
open Bap.Std


exception Parse_error of string

module type Registry = sig
  type t
  val register : string -> t -> unit
  val find : string -> t option
  val list : unit -> (string * t) list
end

module Registry(T : T) = struct
  type t = T.t
  let registered : t String.Table.t = String.Table.create ()
  let register name v = Hashtbl.set registered ~key:name ~data:v
  let find = Hashtbl.find registered
  let list () = Hashtbl.to_alist registered
end


module type Ops = sig
  type t
  module Nullary  : Registry with type t = t
  module Unary : Registry with type t = string -> t
end

module Ops(T : T) = struct
  type t = T.t
  module Nullary = Registry(T)
  module Unary = Registry(struct type t = string -> T.t end)
end

module Predicates = Ops(struct type t = bool Term.visitor end)
module Mappers = Ops(struct type t = Term.mapper end)


let marker parse tag x =
  let x = parse x in
  object
    inherit Term.mapper as super
    method! map_term cls t =
      Term.set_attr t tag x |> super#map_term cls
  end

let has tag = object
  inherit [bool] Term.visitor
  method! enter_term cls t _ = Term.has_attr t tag
end

module Cmp(T : Comparable) = struct
  open T
  let equal x t = x = t
  let greater x t  = x > t
  let lesser  x t  = x < t

  let cmp test tag x cls t _ = match Term.get_attr t tag with
    | None -> false
    | Some y -> test x y

  let make test parse tag x =
    let x = parse x in
    object inherit [bool] Term.visitor
      method! enter_term = cmp test tag x
    end
  let equal = make equal
  let greater = make greater
  let lesser = make lesser
end

let (-) pref tag = pref ^ "-" ^ Value.Tag.name tag
let (+) pref suf = if suf = "" then pref else pref^"-"^suf

let unit suf set is tag =
  Mappers.Nullary.register (set-tag+suf) (marker ident tag ());
  Predicates.Nullary.register (is-tag+suf) (has tag)

module Markers = struct
  module Term = struct
    include Term
    let unit = unit "" "set" "is"

    let () =
      unit synthetic;
      unit live;
      unit dead;
      unit visited;
  end

  module Sub = struct
    include Sub
    let unit = unit "sub" "set" "is"
    let () =
      unit const;
      unit pure;
      unit stub;
      unit extern;
      unit leaf;
      unit malloc;
      unit noreturn;
      unit returns_twice;
      unit nothrow
  end

  module Arg = struct
    include Arg
    let unit = unit "arg" "set" "is"

    let () =
      unit alloc_size;
      unit restricted;
      unit nonnull;
  end

  module Has = struct
    let unit = unit "" "set" "has"
  end

  let () =
    unit "" "set" "has" mark;
end


let expect exp got =
  raise (Parse_error (sprintf "Expected %s got %S" exp got))


module Color = struct
  let colors = [
    "black",   `black;
    "red",     `red;
    "green",   `green;
    "yellow",  `yellow;
    "blue",    `blue;
    "magenta", `magenta;
    "cyan",    `cyan;
    "white",   `white;
  ]

  let grammar = List.map colors ~f:fst |> String.concat ~sep:" | "

  let color_t s = match List.Assoc.find colors s with
    | Some c -> c
    | None -> expect grammar s



  let () =
    let (:=) = Mappers.Unary.register in
    "foreground" := marker color_t foreground;
    "background" := marker color_t background;
    "color"      := marker color_t color

  module Colors = struct
    type t = color
    include Comparable.Make(struct
        type t = color [@@deriving bin_io, compare, sexp]
      end)
  end

  include Cmp(Colors)

  let () =
    let (:=) = Predicates.Unary.register in
    "has-foreground" := equal color_t foreground;
    "has-background" := equal color_t background;
    "has-color"      := equal color_t color
end


module Comment = struct
  let () =
    Mappers.Unary.register "comment" @@
    marker ident comment;
end

module Python = struct
  let () =
    Mappers.Unary.register "python" @@
    marker ident python;
end

module Taint = struct
  let has_attr cmp kind s =
    object inherit [bool] Term.visitor
      method! enter_term cls t _ =
        match Term.get_attr t kind with
        | None -> false
        | Some seed -> match Tid.from_string s with
          | Error _ -> false
          | Ok seed' -> cmp seed seed'
    end

  let either (x : bool Term.visitor) (y : bool Term.visitor) =
    object inherit [bool] Term.visitor
      method! enter_term cls t _ =
        x#visit_term cls t false || y#visit_term cls t false
    end

  let seed tag =
    object inherit Term.mapper as super
      method! map_term cls t  =
        Term.set_attr t tag (Term.tid t) |>
        super#map_term cls
    end


  let has_seed tag  = has_attr Tid.equal tag
  let has_taint tag = has_attr (fun taints taint ->
      Map.exists taints ~f:(fun taints -> Set.mem taints taint)) tag

  let () =
    let (:=) = Predicates.Nullary.register in
    "taints" := either (has Taint.reg) (has Taint.ptr);
    "taints-reg" := has Taint.reg;
    "taints-ptr" := has Taint.ptr;
    "has-taints" := either (has Taint.ptrs) (has Taint.regs);
    "has-tainted-ptr" := has Taint.ptrs;
    "has-tainted-reg" := has Taint.regs

  let () =
    let (:=) = Predicates.Unary.register in
    "taints-reg" := has_seed Taint.ptr;
    "taints-ptr" := has_seed Taint.reg;
    "has-tainted-ptr" := has_taint Taint.ptrs;
    "has-tainted-reg" := has_taint Taint.regs

  let () =
    let (:=) = Mappers.Nullary.register in
    "taint-ptr" := seed Taint.ptr;
    "taint-reg" := seed Taint.reg
end

module True = struct
  let yes = object inherit [bool] Term.visitor
    method! enter_term cls t _ = true
  end

  let () = Predicates.Nullary.register "true" yes
end
