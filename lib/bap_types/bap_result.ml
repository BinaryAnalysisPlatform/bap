open Core_kernel.Std
open Bap_common
open Bap_monad

class type storage = object('s)
  method load : addr -> word option
  method save : addr -> word -> 's
end

type value =
  | Imm of word
  | Mem of storage
  | Bot

module Id : sig
  include Regular
  val zero : t
  val succ : t -> t
end = struct
  type t = Int63.t
  let zero = Int63.zero
  let succ = Int63.succ
  include Regular.Make(struct
      type t = Int63.t with bin_io, compare, sexp
      let pp ppf id =
        Format.fprintf ppf "0x%LX" (Int63.to_int64 id)
      let hash = Int63.hash
      let module_name = Some "Bap.Std.Bil.Result.Id"
      let version = "0.1"
    end)
end

type id = Id.t


type result = {
  id : id;
  v  : value;
}

type t = result
type 'a u = (unit,'a) State.t
type 'a r = (result,'a) State.t

let undefined id =  {id; v = Bot}
let storage s id = {id; v = Mem s}
let word s id = {id; v = Imm s}
let value s = s.v
let id s = s.id

module Storage = struct
  class linear : storage = object
    val storage = []
    method save x u = {< storage = (x,u) :: storage >}
    method load x = List.Assoc.find storage x
  end

  class sparse : storage = object
    val storage = Bitvector.Map.empty
    method save x u = {< storage = Map.add storage ~key:x ~data:u >}
    method load x = Map.find storage x
  end
end

module Value = struct
  type t = value
  include Printable(struct
      type t = value
      let module_name = Some "Bap.Std.Bil.Result.Value"
      let version = "0.1"
      let pp ppf x =
        Format.fprintf ppf "%s" @@ match x with
        | Imm w -> Bap_bitvector.to_string w
        | Mem _ -> "<storage>"
        | Bot   -> "bot";;
    end)
end

include Printable(struct
    type t = result

    let pp ppf r =
      Format.fprintf ppf "[%a] %a" Id.pp (id r) Value.pp (value r);;
    let module_name = Some "Bap.Std.Bil.Result"
    let version = "0.1"

  end)
