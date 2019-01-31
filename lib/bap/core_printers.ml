open Core_kernel
open Bap.Std
open Format


let pp_comma ppf () = pp_print_string ppf ", "

let pair pp_key pp_elem ppf (k,v) =
  fprintf ppf "%a => %a" pp_key k pp_elem v

let pp_map key elem pp_cmp ppf map =
  Seq.pp (pair key elem) ppf (Map.to_sequence map)

let pp_set pp_elem pp_cmp ppf set =
  Seq.pp pp_elem ppf (Set.to_sequence set)


let enum f s = Seq.of_list (f s)

let pp_doubly_linked pp ppf s = Seq.pp pp ppf (Doubly_linked.to_sequence s)
let pp_stack pp ppf s = Seq.pp pp ppf (enum Stack.to_list s)
let pp_heap  pp ppf s = Seq.pp pp ppf (enum Heap.to_list s)
let pp_fheap  pp ppf s = Seq.pp pp ppf (enum Fheap.to_list s)
let pp_bag   pp ppf s = Seq.pp pp ppf (enum Bag.to_list s)
let pp_queue pp ppf s = Seq.pp pp ppf (enum Queue.to_list s)
let pp_fqueue pp ppf s = Seq.pp pp ppf (enum Fqueue.to_list s)
let pp_deque pp ppf s = Seq.pp pp ppf (enum Deque.to_list s)
let pp_fdeque pp ppf s = Seq.pp pp ppf (enum Fdeque.to_list s)
let pp_hashset pp ppf set = Seq.pp pp ppf (enum Hash_set.to_list set)

let pp_hashtbl key elem ppf map =
  Seq.pp (pair key elem) ppf (enum Hashtbl.to_alist map)


let pp_blang pp_elem ppf expr =
  let sexp_of_elem e = sexp_of_string (asprintf "%a" pp_elem e) in
  fprintf ppf "%a" Sexp.pp (Blang.sexp_of_t sexp_of_elem expr)

let install name =
  Pretty_printer.register (sprintf "Core_printers.%s" name)

let () = List.iter ~f:install [
    "pp_map";
    "pp_set";
    "pp_doubly_linked";
    "pp_stack";
    "pp_heap";
    "pp_bag";
    "pp_queue";
    "pp_fqueue";
    "pp_deque";
    "pp_fdeque";
    "pp_hashset";
    "pp_hashtbl";
    "pp_blang"
]
