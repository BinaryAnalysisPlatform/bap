open Core_kernel.Std
open Format

module type Key = sig
  type t
  type token with bin_io, compare, sexp
  val length : t -> int
  val nth_token : t -> int -> token
  val token_hash : token -> int
end

module type S = sig
  type 'a t with bin_io, sexp
  type key
  val create : unit -> 'a t
  val add : 'a t -> key:key -> data:'a -> unit
  val change : 'a t -> key -> ('a option -> 'a option) -> unit
  val find : 'a t -> key -> 'a option
  val remove : 'a t -> key -> unit
  val longest_match : 'a t -> key -> (int * 'a) option
  val length : 'a t -> int
  val pp : (formatter -> 'a -> unit) -> formatter -> 'a t -> unit
end

module Make(Key : Key) = struct
  module Tokens = Hashtbl.Make_binable(struct
      type t = Key.token with bin_io, compare, sexp
      let hash = Key.token_hash
    end)

  type key = Key.t
  type 'a t = {
    data : 'a option;
    subs : 'a t Tokens.t;
  } with bin_io, fields, sexp

  let init v = {data = Some v; subs = Tokens.create ()}
  let create () = {data = None; subs = Tokens.create ()}

  let change goto trie k f =
    let open Option.Monad_infix in
    let len = Key.length k in
    let rec loop n {subs} =
      let c = Key.nth_token k (len - n) in
      match n with
      | 1 -> finish subs c
      | n -> continue subs n c
    and continue parent n c =
      match Tokens.find parent c with
      | Some s -> loop (n-1) s
      | None -> match f None with
        | None -> goto.return ()
        | Some _ ->
          let sub = create () in
          Tokens.add_exn parent c sub;
          loop (n-1) sub
    and finish parent c =
      Tokens.change parent c (function
          | None -> f None >>| init
          | Some t -> Some {t with data = f t.data}) in
    loop len trie

  let change trie k f = with_return (fun goto -> change goto trie k f)
  let add trie ~key ~data:v = change trie key (fun _ -> Some v)
  let remove trie k = change trie k (fun _ -> None)

  let length trie =
    let rec count trie =
      let n = if trie.data = None then 0 else 1 in
      Tokens.fold ~init:n trie.subs
        ~f:(fun ~key:_ ~data:trie n -> n + count trie) in
    count trie

  let longest_match trie k =
    let open Option.Monad_infix in
    let len = Key.length k in
    let rec lookup n trie =
      let c = Key.nth_token k (len - n) in
      match Tokens.find trie.subs c with
      | None -> trie.data >>| fun s -> (len-n, s)
      | Some sub -> match n with
        | 1 -> data sub >>| fun s -> (len,s)
        | n -> lookup (n-1) sub in
    lookup len trie

  let find trie k = match longest_match trie k with
    | None -> None
    | Some (s,v) when s = Key.length k -> Some v
    | Some _ -> None

  let rec pp pp_val fmt t =
    let pp_some_data fmt = function
      | None -> ()
      | Some v -> fprintf fmt "data =@ %a@," pp_val v in
    let pp_table fmt cs =
      Tokens.iter cs (fun ~key ~data ->
          let toks = Key.sexp_of_token key in
          fprintf fmt "@[%a ->@ %a@]"
            Sexp.pp toks (pp pp_val) data) in
    fprintf fmt "{@;@[%a@ %a@]}@;"
      pp_some_data t.data pp_table t.subs
end

module String = Make(struct
    type t = string
    type token = char with bin_io, compare, sexp
    let length = String.length
    let nth_token = String.unsafe_get
    let token_hash = Char.to_int
  end)
