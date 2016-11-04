open Core_kernel.Std

type 'a observation = 'a Univ_map.Key.t
type 'a statement = 'a observation
type 'a t = 'a observation

type ('m,'a) observers = Observers of ('a -> 'm) list

let provide ?inspect name =
  let named s = Sexp.(List [Atom "observation"; Atom name; s]) in
  let sexp_of = match inspect with
    | None -> fun _ -> named @@ Sexp.List []
    | Some f -> fun n -> named @@ f n in
  let k = Univ_map.Key.create ~name sexp_of in
  k,k


module Map = Univ_map.Make1(struct
    type ('a,'m) t = ('a,'m) observers
    let sexp_of_t _ _ = sexp_of_opaque
  end)


type 'e observations = 'e Map.t

let add_observer observers key obs =
  Map.update observers key ~f:(function
      | None -> Observers [obs]
      | Some (Observers observers) ->
        Observers (obs::observers))

let with_observers os key ~f =
  match Map.find os key with
  | None -> f []
  | Some (Observers os) -> f os

let empty = Map.empty
