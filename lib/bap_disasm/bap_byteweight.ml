open Core_kernel.Std
open Bap_types.Std

module type Corpus = sig
  type t
  type key
  val look : t -> length:int -> int -> key option
end

module type S = sig
  type t with bin_io, sexp
  type key
  type corpus

  val create : unit -> t
  val train : t -> max_length:int -> (key -> bool) -> corpus -> unit
  val length : t -> int

  val next : t ->
    length:int ->
    threshold:float ->
    corpus -> int -> int option

  val pp : Format.formatter -> t -> unit
end

module Make
    (Corpus : Corpus)
    (Trie : Trie with type key = Corpus.key) = struct

  module Bin = struct
    type t = (int * int) Trie.t with bin_io, sexp
  end

  type t = Bin.t with bin_io, sexp
  type corpus = Corpus.t
  type key = Corpus.key

  let create = Trie.create

  let pp_pair fmt (x,y) = Format.fprintf fmt "(%d,%d)" x y
  let pp = Trie.pp pp_pair

  let train ~max_length test set pass trie =
    let rec outer = function
      | 0 -> ()
      | n -> inner n 0
    and inner length m = match Corpus.look set ~length m with
      | None -> outer (length - 1)
      | Some s when pass = `Pos && test s ->
        Trie.change trie s (function
            | None -> Some (1,0)
            | Some (a,b) -> Some (a+1,b));
        inner length (m+1)
      | Some s when pass = `Neg ->
        Trie.change trie s (function
            | Some (m,n) when not(test s) -> Some (m,n+1)
            | x -> x);
        inner length (m+1)
      | _ -> inner length (m+1) in
    outer max_length

  let train trie ~max_length test set =
    train ~max_length test set `Pos trie;
    train ~max_length test set `Neg trie

  let test ~threshold trie key =
    match Trie.longest_match trie key with
    | None -> false
    | Some (_,(a,b)) ->
      let n = a + b in
      Float.(of_int a / of_int n) > threshold

  let next trie ~length ~threshold set n =
    let open Option.Monad_infix in
    let rec loop n =
      Corpus.look set ~length n >>= fun key ->
      if test ~threshold trie key then Some n
      else loop (n+1) in
    loop n

  let length = Trie.length
end

module Memory = Bap_memory

type mem = Memory.t

module Bytes = struct
  include Make(struct
      type t = mem
      type key = mem

      let create mem = mem

      let look mem ~length n =
        let from = Addr.(Memory.min_addr mem ++ n) in
        match Memory.view ~from ~words:length mem with
        | Ok mem -> Some mem
        | _ -> None
    end)(Memory.Trie.R8)

  let find bw ~length ~threshold mem =
    let start = Memory.min_addr mem in
    let rec loop acc n =
      match next bw ~length ~threshold mem n with
      | Some n -> loop (Addr.(start ++ n) :: acc) (n+1)
      | None -> List.rev acc in
    loop [] 0

end
