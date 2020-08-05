open Base

type ('k,'d) t = Rel : {
    vals : ('k, ('d,'vo) Set.t, 'ko) Map.t;
    keys : ('d, ('k,'ko) Set.t, 'vo ) Map.t;
  } -> ('k,'d) t

let empty (type key) (type data) compare_key compare_data =
  let module D = struct
    type t = data
    include Comparator.Make(struct
        type t = data
        let compare = compare_data
        let sexp_of_t _ = Sexp.List []
      end)
  end in
  let module K = struct
    type t = key
    include Base.Comparator.Make(struct
        type t = key
        let compare = compare_key
        let sexp_of_t _ = Sexp.List []
      end)
  end in
  Rel {
    vals = Map.empty (module K);
    keys = Map.empty (module D);
  }

let add (Rel {vals; keys}) key value = Rel {
    vals = Map.update vals key ~f:(function
        | Some vals -> Set.add vals value
        | None -> Set.singleton (Map.comparator_s keys) value);
    keys = Map.update keys value ~f:(function
        | Some keys -> Set.add keys key
        | None -> Set.singleton (Map.comparator_s vals) key);
  }

type ('k,'s) non_injective =
  | Non_injective_fwd of 'k list * 's
  | Non_injective_bwd of 's list * 'k

let skips _ _ x = x
let skipu _ x = x


let find_multi xs x = match Map.find xs x with
  | None -> []
  | Some xs -> Set.to_list xs

let matching (Rel {vals; keys}) ?(saturated=skips) ?(unmatched=skipu) init =
  Map.fold ~init vals ~f:(fun ~key ~data:vals init ->
      match Set.to_list vals with
      | [] -> assert false
      | _ :: _ :: _ as vals->
        unmatched (Non_injective_bwd (vals,key)) init
      | [s] -> match find_multi keys s with
        | [_] -> saturated key s init
        | xs -> unmatched (Non_injective_fwd (xs,s)) init)

let fold (Rel {vals}) ~init ~f =
  Map.fold vals ~init ~f:(fun ~key:left ~data:rights init ->
      Set.fold ~init rights ~f:(fun init right -> f left right init))

let iter rels ~f = fold rels ~init:() ~f:(fun x s () -> f x s)

let is_empty (Rel {vals}) = Map.is_empty vals
let findl (Rel {vals}) = find_multi vals
let findr (Rel {keys}) = find_multi keys
let mem (Rel {vals; keys}) x s = Map.mem vals x && Map.mem keys s
