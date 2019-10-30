open Core_kernel
open Regular.Std
open Format
open Option.Monad_infix
open Bap_trie_intf

module Make(Key : Key) = struct
  module Tokens = Hashtbl.Make_binable(struct
      type t = Key.token [@@deriving bin_io, compare, sexp]
      let hash = Key.token_hash
    end)

  type key = Key.t
  type token = Key.token

  type 'a t = {
    mutable data : 'a option;
    subs : 'a t Tokens.t;
  } [@@deriving bin_io, fields, sexp]

  let init v = {data = Some v; subs = Tokens.create ()}
  let create () = {data = None; subs = Tokens.create ()}

  let change trie k f {return} =
    let len = Key.length k in
    let rec loop n {subs} =
      let c = Key.nth_token k n in
      if n + 1 = len then found subs c else find subs n c
    and found parent c = Tokens.change parent c (function
        | None -> f None >>| init
        | Some t -> Some {t with data = f t.data})
    and find parent n c = match Tokens.find parent c with
      | Some s -> loop (n+1) s
      | None -> match f None with
        | None -> return ()
        | Some _ ->
          let sub = create () in
          Tokens.add_exn parent c sub;
          loop (n+1) sub in
    if len > 0 then loop 0 trie
    else trie.data <- f trie.data

  let walk root k ~init ~f  =
    let len = Key.length k in
    let rec lookup best n trie =
      match Tokens.find trie.subs (Key.nth_token k n) with
      | None -> best
      | Some sub ->
        let best = f best sub.data in
        if n + 1 = len then best else lookup best (n+1) sub in
    let best = f init root.data in
    if len > 0 then lookup best 0 root else best

  let length trie =
    let rec count trie =
      let n = if trie.data = None then 0 else 1 in
      Tokens.fold ~init:n trie.subs
        ~f:(fun ~key:_ ~data:trie n -> n + count trie) in
    count trie

  let longest_match root k =
    walk root k ~init:(0,0,None) ~f:(fun (n,i,best) -> function
        | None -> (n+1,i,best)
        | better -> (n+1,n,better)) |> function
    | (_,i,Some thing) -> Some (i,thing)
    | _ -> None

  let change trie k f = with_return (change trie k f)
  let add trie ~key ~data:v = change trie key (fun _ -> Some v)
  let remove trie k = change trie k (fun _ -> None)

  let find trie k = match longest_match trie k with
    | Some (s,v) when s = Key.length k -> Some v
    | _ -> None



  let fold t ~init ~f =
    let rec fold init rprefix t =
      let init = match t.data with
        | None -> init
        | Some data -> f init (List.rev rprefix) data in
      fold_table init rprefix t.subs
    and fold_table init rprefix cs =
      Tokens.fold cs ~init ~f:(fun ~key ~data init ->
          let rprefix = key :: rprefix in
          fold init rprefix data) in
    fold init [] t

  let iter t ~f = fold t ~init:() ~f:(fun () tokens data ->
      f tokens data)

  let make_printer pp_prefix pp_val ppf t =
    let rec pp_trie ppf rprefix t =
      Option.iter t.data ~f:(fun data ->
          fprintf ppf "@[<2>%a@ %a@]@;"
            pp_val data pp_prefix (List.rev rprefix));
      pp_table ppf rprefix t.subs
    and pp_table ppf rprefix cs =
      Tokens.iteri cs (fun ~key ~data ->
          let rprefix = key :: rprefix in
          pp_trie ppf rprefix data) in
    pp_trie ppf [] t

  let pp_sexps ppf tokens =
    let tokens = List.map tokens ~f:Key.sexp_of_token in
    Sexp.pp ppf (Sexp.List tokens)

  let pp pp_val = make_printer pp_sexps pp_val
end

module String = struct
  module Common = struct
    type t = string
    type token = char [@@deriving bin_io, compare, sexp]
    let length = String.length
    let token_hash = Char.to_int
  end
  module Prefix = Make(struct
      include Common
      let nth_token = String.get
    end)

  module Suffix = Make(struct
      include Common
      let nth_token s n =
        s.[String.length s - n - 1]
    end)
end

module Array = struct
  module Common(T : Token) = struct
    type t = T.t array
    type token = T.t [@@deriving bin_io, compare, sexp]
    let length = Array.length
    let token_hash = T.hash
  end
  module Prefix(T : Token) = Make(struct
      include Common(T)
      let nth_token = Array.get
    end)

  module Suffix(T : Token) = Make(struct
      include Common(T)
      let nth_token xs n = xs.(Array.length xs - n - 1)
    end)
end
