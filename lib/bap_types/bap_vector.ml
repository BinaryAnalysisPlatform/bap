open Core_kernel
open Regular.Std
open Format

type 'a t = {
  mutable size : int;
  mutable data : 'a array;
  default : 'a;
} [@@deriving bin_io, compare, sexp]

let create ?(capacity=16) default =
  let capacity = max capacity 1 in
  {
    size = 0;
    data = Array.create ~len:capacity default;
    default;
  }

let resize vec =
  let n = Array.length vec.data in
  let data = Array.init (vec.size * 2)
      ~f:(fun i -> if i < n
           then Array.unsafe_get vec.data i
           else vec.default) in
  vec.data <- data

let append vec x =
  if Array.length vec.data = vec.size
  then resize vec;
  Array.unsafe_set vec.data vec.size x;
  vec.size <- vec.size + 1

let to_array vec =
  Array.sub vec.data ~pos:0 ~len:vec.size

let map_to_array vec ~f =
  Array.init vec.size ~f:(fun i ->
      f (Array.unsafe_get vec.data i))

let get vec n =
  if n < vec.size then Array.unsafe_get vec.data n
  else invalid_arg "Index out of bounds"

let set vec n x =
  if n < vec.size then Array.unsafe_set vec.data n x
  else invalid_arg "Index out of bounds"

let nth vec n =
  if n < vec.size then Some (Array.unsafe_get vec.data n)
  else None


include struct
  open Container.Continue_or_stop

  let fold_until vec ~init ~f ~finish =
    let rec loop i init =
      if i < vec.size
      then match f init (Array.unsafe_get vec.data i) with
        | Stop x -> x
        | Continue r -> loop (i+1) r
      else finish init in
    loop 0 init

  let fold_result vec ~init ~f =
    let rec loop i init =
      if i < vec.size
      then match f init (Array.unsafe_get vec.data i) with
        | Error _ as e -> e
        | Ok r -> loop (i+1) r
      else Ok init in
    loop 0 init
end


let foldi vec ~init ~f =
  let rec loop i init =
    if i < vec.size
    then loop (i+1) (f i init (Array.unsafe_get vec.data i))
    else init in
  loop 0 init

let fold vec ~init ~f =
  foldi vec ~init ~f:(fun _ acc x -> f acc x)

let iteri vec ~f =
  let rec loop i =
    if i < vec.size
    then
      let () = f i (Array.unsafe_get vec.data i) in
      loop (i+1)  in
  loop 0


let iter vec ~f = iteri vec ~f:(fun _ x -> f x)

let find_mapi vec ~f =
  with_return (fun {return} ->
      iteri vec ~f:(fun i x -> match f i x with
          | None -> ()
          | hay -> return hay);
      None)

let findi vec ~f =
  with_return (fun {return} ->
      iteri vec ~f:(fun i x ->
          if f i x then return (Some (i,x)));
      None)

let peq = Polymorphic_compare.equal

let index_with ?(equal=peq) ~default vec x : int =
  with_return (fun {return} ->
      iteri vec ~f:(fun i y -> if equal x y then return i);
      default)

let index ?equal vec x : int option =
  let n = index_with ~default:(-1) ?equal vec x in
  if n < 0 then None else Some n

let index_exn ?equal vec x : int =
  let n = index_with ~default:(-1) ?equal vec x in
  if n < 0 then raise Not_found else n

module C = Container.Make(struct
    type nonrec 'a t = 'a t
    let fold = fold
    let iter = `Custom iter
  end)

let mem = C.mem
let length vec = vec.size
let is_empty vec = length vec = 0
let exists = C.exists
let for_all = C.for_all
let count = C.count
let sum = C.sum
let find = C.find
let find_map = C.find_map

let to_list vec =
  List.init vec.size (fun i ->
      Array.unsafe_get vec.data i)

let min_elt = C.min_elt
let max_elt = C.max_elt

let pp pp_elem ppf vec =
  let n = length vec in
  fprintf ppf "{@[<2>";
  iteri vec ~f:(fun i x ->
      if i < n - 1 then fprintf ppf "%a;@ " pp_elem x);
  if n > 0 then fprintf ppf "%a" pp_elem vec.data.(n-1);
  fprintf ppf "@]}"

let () = Pretty_printer.register "Bap.Std.Vector.pp"
