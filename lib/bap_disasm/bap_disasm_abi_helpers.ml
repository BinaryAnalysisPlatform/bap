(*  *)
open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_abi


class stub : abi = object
  method id = ["unknown"; "unknown"; "unknown"]
  method specific = false
  method choose _ = -1
  method return_value = None
  method args = []
  method vars = []
  method records = []
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

let join x y : abi = object
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

let create_abi_getter (registered : abi_constructor list ref) =
  fun ?(merge=merge) ?image ?sym mem blk ->
    List.filter_map !registered (fun cs ->
        try Some (cs ?image ?sym mem blk) with exn -> None) |>
    List.sort ~cmp |> function
    | [] -> new stub
    | x :: _ as xs ->
      merge (List.take_while xs ~f:(fun y -> cmp x y = 0))
