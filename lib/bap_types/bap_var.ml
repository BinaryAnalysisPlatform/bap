open Core_kernel.Std
open Regular.Std
open Bap_common

module Id = struct
  include Bap_state.Make(struct
      type t = Int63.t ref
      let create () = ref Int63.zero
    end)
  let create () =
    let id = !state in
    Int63.incr id;
    !id
end

module T = struct
  type t = {
    var : string;
    ind : int;
    typ : typ;
    vir : bool;
  } with sexp, bin_io,compare

  let hash v = String.hash v.var
  let module_name = Some "Bap.Std.Var"
  let version = "0.1"
  let pp fmt v =
    Format.fprintf fmt "%s%s" v.var
      (if v.ind <> 0 then sprintf ".%d" v.ind else "" )
end

include T

let name v = v.var
let with_index v ind = {v with ind}
let index v = v.ind
let base v = {v with ind = 0}
let typ  v = v.typ
let is_physical v = not v.vir
let is_virtual v = v.vir


let create ?(is_virtual=false) ?(fresh=false) name typ =
  let var =
    if fresh then name ^ Int63.to_string (Id.create ())
    else name in
  {ind = 0; var; typ; vir = is_virtual}

let same x y = base x = base y

include Regular.Make(T)
