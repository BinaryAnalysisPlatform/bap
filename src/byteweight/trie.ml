open Core_kernel.Std
open Trie_common

module MakeKey (Unit : Hashtbl.Key) : (K with type key = Unit.t) = struct
  module Table = Hashtbl.Make(Unit)
  type 'a t = 'a Table.t
  type key = Table.key
  let create ()= Table.create ()
  let iter f map = Table.iter map ~f:(fun ~key ~data -> f key data)
  let add map key data = Table.replace map ~key:key ~data:data
  let find = Table.find
  let replace = add
  let string_of_key a = Sexp.to_string_hum (Unit.sexp_of_t a)
end

module Make (Key : K) : (T with type key = Key.key list) = struct
  type key = Key.key list
  type 'a t = Node of 'a * 'a t Key.t
  let init v = Node (v, Key.create ())
  let rec add trie k v = match k with
    | [] -> ()
    | hd :: [] -> (
        match trie with
        | Node (_, m) ->
          match (Key.find m hd) with
            | Some (Node (_, sub)) -> Key.replace m hd (Node (v, sub))
            | None ->
              (* If this is a new node, add to its father node's map *)
              let subtrie_init = init v in
              Key.add m hd subtrie_init
      )
    | hd :: tl ->
      match trie with
      | Node (_, m) ->
        let subtrie = match Key.find m hd with
          | Some s -> s
          | None ->
            (* If this is a new node, add to its father node's map *)
            let subtrie_init = init v in
            Key.add m hd subtrie_init;
            subtrie_init in
        add subtrie tl v
  let find trie k =
    let rec rec_find trie k t_v = match k with
      | [] -> t_v
      | hd :: tl ->
        match trie with
        | Node (_, m) ->
          match Key.find m hd with
          | Some (Node (v, _) as subtrie) ->
            rec_find subtrie tl v
          | None -> t_v in
    let root_v = match trie with
      | Node (v, _) -> v in
    rec_find trie k root_v
  let output trie file format_v =
    let oc = open_out file in
    let rec rec_output prefix = function
      | Node (v, m) ->
        let s = Printf.sprintf "%s->%s\n"
                  (String.concat ~sep:";" (List.rev prefix))
                  (format_v v) in
        Out_channel.output_string oc s;
        Key.iter (fun k v ->
            rec_output (Key.string_of_key k :: prefix) v
        ) m in
    rec_output [] trie;
    Out_channel.close oc
end
