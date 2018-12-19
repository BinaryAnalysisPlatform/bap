open Core_kernel

module Domain = Bap_knowledge_domain
module Label = Bap_knowledge_label

type 'a serializer = (module Binable.S with type t = 'a)

type 'a tinfo = {
  domain : (module Domain.S with type t = 'a);
  typeid : string;
  serops : 'a serializer;
}

type 'a value = {
  value : 'a;
  tinfo : 'a tinfo;
}

module Dict = Univ_map.Make(struct
    type 'a t = 'a value
    let sexp_of_t _ x = sexp_of_opaque x
  end)

module Key = Type_equal.Id

type 'a domain = {
  key : 'a Key.t;
  typ : 'a tinfo;
}


type t = Dict.t
type semantics = t

let parsers = String.Table.create ()
let domains = ref []

let store_parser (type a)
    (module S : Binable.S with type t = a) {key; typ} =
  let name = Key.name key in
  if Hashtbl.mem parsers name
  then invalid_argf "A serializable semantics must have a unique name.
      The following semantics data type is not unique: %s" name ();
  let parser data =
    let value = Binable.of_string (module S) data in
    Dict.Packed.T (key, {value; tinfo=typ}) in
  Hashtbl.set parsers ~key:name ~data:parser

let empty_serializer (type a) (module D : Domain.S with type t = a) =
  let module S = struct
    type t = a
    include Binable.Of_binable(Unit)(struct
        type t = a
        let to_binable _ = ()
        let of_binable () = D.empty
      end)
  end  in
  (module S : Binable.S with type t = a)




let declare (type t) ?serializer
    ~name
    (module T : Domain.S with type t = t) =
  let serops = match serializer with
    | None -> empty_serializer (module T)
    | Some s -> s in
  let domain = {
    key = Key.create ~name (fun _ -> Sexp.Atom name);
    typ = {
      typeid = name;
      domain = (module T);
      serops;
    };
  } in
  if Option.is_some serializer
  then store_parser serops domain;
  domains := name :: !domains;
  domain


let empty = Dict.empty
let domain s = s.typ.domain

let put : type s. s domain -> t -> s -> t =
  fun {key; typ=tinfo} data value ->
    Dict.set data key {value; tinfo}

let create d x = put d empty x

let get : type s. s domain -> t -> s =
  fun {key; typ = {domain = (module T)}} data ->
    match Dict.find data key with
    | None -> T.empty
    | Some x -> x.value

let merge_value : type a. a value -> a value -> a value =
  fun {value=x; tinfo={domain=(module D)}} ({value=y} as r) ->
    match D.partial x y with
    | LE | EQ | NC -> r
    | GE -> {r with value=x}

let merge : t -> t -> t = fun x y ->
  Dict.to_alist x |>
  List.fold ~init:y ~f:(fun v (Dict.Packed.T (k,x)) ->
      Dict.update v k ~f:(function
          | None -> x
          | Some y -> merge_value y x))

let (<:=) x y =
  Dict.to_alist x |> List.for_all ~f:(function
        Dict.Packed.T (k,{value=x; tinfo={domain=(module V)}}) ->
        match Dict.find y k with
        | None -> false
        | Some {value=y} -> V.partial x y = LE)

let partial : t -> t -> Domain.Order.partial = fun x y ->
  match x <:= y, y <:= x with
  | true,false  -> LE
  | true,true   -> EQ
  | false,true  -> GE
  | false,false -> NC


let compare x y = match partial x y with
  | EQ -> 0
  | GE -> 1
  | _ -> -1

let inspect x =
  Sexp.List (Dict.to_alist x |> List.map ~f:(function
      | Dict.Packed.T (_,{value; tinfo={domain=(module V); typeid}}) ->
        Sexp.List [
          Sexp.Atom typeid;
          V.inspect value
        ]))

let sexp_of_t = inspect
let t_of_sexp _ = empty



module Sorted(Eq : T1) = struct
  type 'a s = 'a Eq.t
  type 'a t = {
    sort : 'a s;
    data : semantics;
  }
  let create sort data = {sort; data}
  let empty sort = {sort; data = empty}
  let put domain v value = {v with data = put domain v.data value}
  let get domain v = get domain v.data
  let partial x y = partial x.data y.data
  let merge x y = {y with data = merge x.data y.data}
  let kind v = v.sort
  let semantics x = x.data
end

module Repr = struct
  type entry = {
    name : string;
    data : string;
  } [@@deriving bin_io]

  type t = entry list [@@deriving bin_io]
end

include Binable.Of_binable(Repr)(struct
    type t = semantics
    let to_binable s =
      Dict.to_alist s |>
      List.rev_map ~f:(fun (Dict.Packed.T (k,{
          value=x;
          tinfo={serops}
        })) -> Repr.{
          name = Key.name k;
          data = Binable.to_string serops x;
        })

    let of_binable entries =
      List.fold entries ~init:empty ~f:(fun s {Repr.name; data} ->
          match Hashtbl.find parsers name with
          | None -> s
          | Some parse ->
            let Dict.Packed.T (key,data) = parse data in
            Dict.set s key data)
  end)


let domains () = !domains


open Format
let pp_sep ppf () = pp_print_break ppf 1 2

let rec pp_sexp ppf = function
  | Sexp.Atom x -> fprintf ppf "%a" pp_print_text x
  | Sexp.List xs ->
    fprintf ppf "@[<2>(%a)@]"
      (pp_print_list ~pp_sep pp_sexp)  xs


let pp ppf x = pp_sexp ppf (inspect x)

let pp_domains constraints ppf x =
  let constraints = String.Set.of_list constraints in
  let single = Set.length constraints = 1 in
  match inspect x with
  | Atom _ -> assert false
  | List domains -> List.iter domains ~f:(function
      | Sexp.List [Sexp.Atom name; data] as pair ->
        if Set.mem constraints name
        then fprintf ppf "@[<2>%a@]" pp_sexp
            (if single then data else pair);
        fprintf ppf "@\n";
      | _ -> assert false)
