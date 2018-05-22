open Core_kernel.Std


module type Key = sig 
  type t
  val compare : t -> t -> int
  val null : t 
  val succ : t -> t
end

module Persistent = struct 
  module type S = sig
    type t 
    type key
    val empty : t
    val string : t -> key -> string
    val key : t -> string -> key
    val register : t -> string -> t
    val registered : t -> string -> bool
  end
  module Make(Key : Key) = struct 
    module Index = Map.Make(struct 
        include Key
        let sexp_of_t = sexp_of_opaque
        let t_of_sexp = opaque_of_sexp
      end)

    type key = Key.t

    type t = {
      strings : string Index.t;
      keys : key String.Map.t;
    }

    let empty = {
      strings = Index.empty;
      keys = String.Map.empty;
    }

    let string {strings} key = 
      try Map.find_exn strings key 
      with Not_found -> ""

    let key {keys} str = 
      try Map.find_exn keys str 
      with Not_found -> Key.null

    let register idx str = 
      if Map.mem idx.keys str then idx
      else 
        let key = Key.succ @@ match Map.max_elt idx.strings with
          | None -> Key.null 
          | Some (k,_) -> k in
        {
          strings = Map.add idx.strings ~key ~data:str;
          keys = Map.add idx.keys ~key:str ~data:key
        } 

    let registered {keys} str = Map.mem keys str
  end
end

