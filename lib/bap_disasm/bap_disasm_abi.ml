open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_ir

type def = var * exp

class virtual t = object
  method virtual id : string list
  method specific = false
  method choose (_:t) = -1
  method return_value : def option = None
  method args : def list= []
  method vars : def list = []
  method records : def list list = []
end

class stub = object
  inherit t
  method id = ["unknown"; "unknown"; "unknown"]
end

let merge_id x y =
  let rec loop acc x y = match x,y with
    | [],[] -> List.concat (List.rev acc)
    | x::xs,[]|[],x::xs -> loop ([x]::acc) xs []
    | x::xs, y::ys -> loop ([x;y]::acc) xs ys in
  loop [] x y |> List.remove_consecutive_duplicates
    ~equal:String.equal


let either_or_first x y = match x,y with
  | [],x | x,[] -> x
  | x,y -> x

let join x y : t = object
  inherit stub as stub
  method id = merge_id x#id y#id
  method specific = x#specific || y#specific
  method choose other = max (x#choose other) (y#choose other)
  method return_value =
    if x#return_value = stub#return_value
    then x#return_value
    else y#return_value
  method args = either_or_first x#args y#args
  method vars = either_or_first x#vars y#vars
  method records = either_or_first x#records y#records
end

let merge = function
  | [] -> new stub
  | xs -> List.reduce_exn xs ~f:join


let to_string arch abi =
  Arch.to_string arch :: List.rev abi |> String.concat ~sep:"-"


let cmp x y = match x#choose y, y#choose x with
  | p,q when p = q -> 0
  | p,_ -> p

let abis : (arch, (sub term -> t) list) Hashtbl.t =
  Arch.Table.create ()

let create ?(merge=merge) arch (sub : sub term) : t =
  let registered = match Hashtbl.find abis arch with
    | Some r -> r
    | None -> [] in
  List.filter_map registered (fun cs ->
      try Some (cs sub) with exn -> None) |>
  List.sort ~cmp |> function
  | [] -> new stub
  | x :: _ as xs ->
    merge (List.take_while xs ~f:(fun y -> cmp x y = 0))


let register arch new_abi =
  Hashtbl.add_multi abis ~key:arch ~data:new_abi
