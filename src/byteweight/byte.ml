open Core_kernel.Std
open Or_error
open Bap.Std

exception Sig_incomplete
exception Format_error

module Byte : (Mode.S with type t = char) = struct
  type t = char
  type key = t list

  module TList = struct
    type t = key
    let to_str = String.of_char_list
    let to_hex_str k =
      let ascii_list = List.map k ~f:(fun c -> Printf.sprintf "0x%x" @@
      Char.to_int c) in
      String.concat ~sep:" " ascii_list
    let hash k = String.hash (to_str k)
    let compare a b = String.compare (to_str a) (to_str b)
    let t_of_sexp sexp =
      let str = String.t_of_sexp sexp in
      List.rev (String.to_list_rev str)
    let sexp_of_t k = String.sexp_of_t (to_str k)
  end
  (* TODO: replace signature table with trie *)
  module Table = Hashtbl.Make(TList)
  (* TODO: make trie easier *)
  module Tree = Trie.Make(Trie.MakeKey(Char))

  let trie = Tree.init 0.0
  let find = Tree.find trie
  let len = 30

  let generate_keys from ?(len=len) mem : key list =
    let max_key_rev =
      let sub_mem =
        (* TODO: report the bug when len is too long for memory *)
        let len =
          let sec_nd = Memory.max_addr mem in
          if Addr.(from ++ len) >= sec_nd then
            match (Addr.to_int sec_nd), (Addr.to_int from) with
            | Ok x, Ok y -> x - y
            | _ -> failwith "Addr conversion error"
          else len in
        match Memory.view ~from:from ~words:len mem with
        | Ok m -> m
        | Error _ -> mem in
      let mem_str = Bigsubstring.to_string (Memory.to_buffer sub_mem) in
      String.to_list_rev mem_str in
    let rec rec_g res key_rev =
      match key_rev with
      | [] -> List.map res ~f:List.rev
      | _ :: tl -> rec_g (key_rev :: res) tl in
    rec_g [] max_key_rev

  let split_strsep_last ~on str =
    let pos_list = String.substr_index_all str ~may_overlap:false ~pattern:on in
    match List.max_elt ~cmp:Int.compare pos_list with
    | Some pos ->
      let bytes_str = String.slice str 0 pos in
      let len_sep = String.length on in
      let counts = String.slice str (pos + len_sep) (String.length str) in
      [bytes_str;counts]
    | None -> raise Sig_incomplete

  let to_bytes_score line =
    let words = split_strsep_last ~on:"->" line in
    match words with
    | [bytes_str; counts] ->
      let p, n =
        let p_n = String.split ~on:',' counts in
        match p_n with
        | [p;n] -> Float.of_string p, Float.of_string n
        | _ -> raise Format_error in
      List.rev (String.to_list_rev bytes_str), (p /. (p +. n))
    | _ -> raise Format_error

  let read_sig file =
    let ic = In_channel.create file in
    let init_prefix = "" in
    Seq.unfold_step ~init:init_prefix ~f:(fun prefix ->
      match In_channel.input_line ic with
      | None -> In_channel.close ic; Seq.Step.Done
      | Some line ->
        let merged_line =
          if prefix = init_prefix then prefix ^ line
          else prefix ^ "\n" ^ line in
        try
          let bytes, score = to_bytes_score merged_line in
          Seq.Step.Yield ((bytes, score), init_prefix)
        with
          | Sig_incomplete -> Seq.Step.Skip merged_line
          | Format_error ->
            Printf.fprintf stderr "WPT File Format error, skip line %s\n" line;
            Seq.Step.Skip init_prefix
    )

  let load file =
    let sigs = read_sig file in
    Seq.iter sigs ~f:(fun (k, v) -> Tree.add trie k v)

  let consecutive ?arch byte_list =
    if List.length byte_list < len then byte_list
    else List.sub byte_list ~pos:0 ~len:len

  let string_of_key = TList.to_str
end

include Byte
