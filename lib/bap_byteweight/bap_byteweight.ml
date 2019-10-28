open Core_kernel
open Bap.Std

module type Corpus = sig
  type t
  type key
  val look : t -> length:int -> int -> key option
end

module type S = sig
  type t [@@deriving bin_io, sexp]
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
    (Trie : Trie.S with type key = Corpus.key) = struct

  module Bin = struct
    type t = (int * int) Trie.t [@@deriving bin_io, sexp]
  end

  type t = Bin.t [@@deriving bin_io, sexp]
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

  let next_if (trie : t) ~length ~f set n =
    let open Option.Monad_infix in
    let rec loop n =
      Corpus.look set ~length n >>= fun key ->
      match Trie.longest_match trie key with
      | None -> loop (n+1)
      | Some (len,stats) ->
        if f key len stats
        then Some n
        else loop (n+1) in
    loop n


  let next trie ~length ~threshold set n =
    next_if trie ~length set n ~f:(fun _ _ (a,b) ->
        let n = a + b in
        Float.(of_int a / of_int n) > threshold)

  let length = Trie.length
end

module Make2
    (Corpus : Corpus)
    (Trie : Trie.V2.S with type key = Corpus.key) = struct
  include Make(Corpus)(Trie)
  type token = Trie.token
  let fold = Trie.fold
end

module Bytes = struct
  include Make2(struct
      type t = mem
      type key = mem

      let create mem = mem

      let look mem ~length n =
        let from = Addr.(Memory.min_addr mem ++ n) in
        match Memory.view ~from ~words:length mem with
        | Ok mem -> Some mem
        | _ -> None
    end)(Memory.Trie.Stable.V1.R8)


  let find bw ~length ~threshold mem =
    let start = Memory.min_addr mem in
    let rec loop acc n =
      match next bw ~length ~threshold mem n with
      | Some n -> loop (Addr.(start ++ n) :: acc) (n+1)
      | None -> List.rev acc in
    loop [] 0

  let find_if bw ~length ~f mem =
    let start = Memory.min_addr mem in
    let rec loop acc n =
      match next_if bw ~length ~f mem n with
      | Some n -> loop (Addr.(start ++ n) :: acc) (n+1)
      | None -> List.rev acc in
    loop [] 0

  let p1 m n = float m /. float (m + n)
  and p0 m n = float n /. float (m + n)

  let find_using_bayes_factor sigs ~min_length ~max_length threshold =
    let (s1,s0) = fold sigs ~init:(0,0) ~f:(fun (s1,s0) key (h1,h0) ->
        let length = List.length key in
        if length >= min_length && length <= max_length then
          h1 + s1, h0 + s0
        else s1,s0) in
    let ph1 = float s1 /. float (s1 + s0) in
    let ph0 = 1. -. ph1 in
    let ratio m n =
      let r = p1 m n /. p0 m n
      and q = ph1 /. ph0 in
      r *. q in
    find_if sigs ~length:max_length ~f:(fun _ length (h1,h0) ->
        length >= min_length &&
        Float.(ratio h1 h0 > threshold))

  let find_using_threshold sigs ~min_length ~max_length threshold =
    find_if sigs ~length:max_length ~f:(fun _ length (h1,h0) ->
        length >= min_length &&
        Float.(p1 h1 h0 > threshold))

  let pp_byte ppf x =
    Format.fprintf ppf "%02x" @@ Word.to_int_exn x

  let pp ppf t = fold t ~init:() ~f:(fun () words (a,b) ->
      let p1h1 = float a /. float (a+b) in
      Format.fprintf ppf "%-8d %-8d %-8d %.4f " (a+b) a b p1h1;
      List.iter words ~f:(Format.fprintf ppf "%a" pp_byte);
      Format.fprintf ppf "@\n");
end

type stats = int * int

module V1 = struct
  module type S = S
  module Make = Make
end
module V2 = struct
  module type S = sig
    include V1.S

    type token

    val next_if : t -> length:int -> f:(key -> int -> stats -> bool) -> corpus ->
      int -> int option

    val fold : t -> init:'b -> f:('b -> token list -> stats -> 'b) -> 'b
  end

  module Make = Make2
end

module Stats = struct
  type t = stats
  let trials (a,b) = a + b
  let h1 = fst
  let h0 = snd
end
