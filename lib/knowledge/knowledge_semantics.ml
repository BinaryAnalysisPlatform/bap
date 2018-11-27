open Core_kernel

module Domain = Knowledge_domain
module Label = Knowledge_label

type 'a tinfo = {
  domain : (module Domain.S with type t = 'a);
  typeid : string;
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

let declare (type t) ~name
    (module T : Domain.S with type t = t) = {
  key = Key.create ~name (fun _ -> Sexp.Atom name);
  typ = {
    typeid = name;
    domain = (module T);
  };
}

let empty = Dict.empty
let domain s = s.typ.domain

let put : type s. s domain -> t -> s -> t =
  fun {key; typ=tinfo} data value ->
    Dict.set data key {value; tinfo}

let get : type s. s domain -> t -> s =
  fun {key; typ = {domain = (module T)}} data ->
    match Dict.find data key with
    | None -> T.empty
    | Some x -> x.value

let merge : t -> t -> t = fun x y ->
  Dict.to_alist x |>
  List.fold ~init:y ~f:(fun v (Dict.Packed.T (k,x)) ->
      if Dict.mem v k then v
      else Dict.set v k x)

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

let inspect x =
  Sexp.List (Dict.to_alist x |> List.map ~f:(function
      | Dict.Packed.T (_,{value; tinfo={domain=(module V); typeid}}) ->
        Sexp.List [
          Sexp.Atom typeid;
          V.inspect value
        ]))


module Sorted(Eq : T1) = struct
  type 'a s = 'a Eq.t
  type 'a t = {
    sort : 'a s;
    data : semantics;
  }
  let empty sort = {sort; data = empty}
  let put domain v value = {v with data = put domain v.data value}
  let get domain v = get domain v.data
  let partial x y = partial x.data y.data
  let merge x y = {y with data = merge x.data y.data}
  let kind v = v.sort
  let semantics x = x.data
end
