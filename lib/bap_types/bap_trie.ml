open Core_kernel[@@warning "-D"]
open Regular.Std
open Format
open Option.Monad_infix
open Bap_trie_intf

module Make(Key : Key) = struct
  module Token_key = struct
    type t = Key.token [@@deriving bin_io, compare, sexp]
    let hash = Key.token_hash
  end

  (* Simple assoc-list based table (v0.17 compat) *)
  module Tokens = struct
    type 'a t = (Token_key.t * 'a) list ref

    let create () : 'a t = ref []

    let find t k =
      let rec loop = function
        | [] -> None
        | (k',v) :: rest ->
          if Base.Poly.equal k k' then Some v else loop rest in
      loop !t

    let change t k ~f =
      let rec loop acc = function
        | [] -> (match f None with
            | Some v -> List.rev ((k,v) :: acc)
            | None -> List.rev acc)
        | (k',v) :: rest ->
          if Base.Poly.equal k k'
          then (match f (Some v) with
              | Some v' -> List.rev acc @ [(k',v')] @ rest
              | None -> List.rev acc @ rest)
          else loop ((k',v) :: acc) rest in
      t := loop [] !t

    let add_exn t ~key ~data = t := (key,data) :: !t

    let fold t ~init ~f = List.fold !t ~init ~f:(fun acc (k,v) -> f ~key:k ~data:v acc)

    let iteri t ~f = List.iter !t ~f:(fun (k,v) -> f ~key:k ~data:v)

    let sexp_of_t sa t =
      Sexp.List (List.map !t ~f:(fun (k,v) -> Sexp.List [Token_key.sexp_of_t k; sa v]))

    let t_of_sexp sa = function
      | Sexp.List items ->
        ref (List.map items ~f:(function
            | Sexp.List [k;v] -> (Token_key.t_of_sexp k, sa v)
            | _ -> failwith "Trie.Sexp: bad pair"))
      | _ -> failwith "Trie.Sexp: expected list"

    module A = struct
      type 'a t = (Token_key.t * 'a) list [@@deriving bin_io]
    end

    let bin_shape_t sa = A.bin_shape_t sa
    let bin_size_t sa (t : 'a t) = A.bin_size_t sa !t
    let bin_write_t wa (buf : Bin_prot.Common.buf) ~(pos:int) (t : 'a t) = A.bin_write_t wa buf ~pos !t
    let bin_read_t ra buf ~pos_ref = ref (A.bin_read_t ra buf ~pos_ref)
    let __bin_read_t__ ra buf ~pos_ref = ref (A.__bin_read_t__ ra buf ~pos_ref)
    let bin_writer_t wa = A.bin_writer_t wa
    let bin_reader_t ra = A.bin_reader_t ra
    let bin_t tc = A.bin_t tc
  end

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
    and found parent c = Tokens.change parent c ~f:(function
        | None -> f None >>| init
        | Some t -> Some {t with data = f t.data})
    and find parent n c = match Tokens.find parent c with
      | Some s -> loop (n+1) s
      | None -> match f None with
        | None -> return ()
        | Some _ ->
          let sub = create () in
          ignore (Tokens.add_exn parent ~key:c ~data:sub);
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
      let n = if Option.is_none trie.data then 0 else 1 in
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
      Tokens.iteri cs ~f:(fun ~key ~data ->
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
