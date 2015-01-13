open Core_kernel.Std

include List

let create x xs = x::xs
let singleton x = [x]
let of_list = function
  | [] -> None
  | x  -> Some x

external to_list : 'a t -> 'a list = "%identity"

let hd = hd_exn
let tl = tl_exn
let last = last_exn
let reduce = reduce_exn
