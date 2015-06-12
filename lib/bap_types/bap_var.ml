open Core_kernel.Std
open Bap_common

module Id = struct
  type t = Int63.t
  let id = ref Int63.zero

  let create () =
    Int63.incr id;
    !id
end

module T = struct
  type t = {
    var : string;
    ver : int;
    typ : typ;
    tmp : bool;
  } with sexp, bin_io,compare

  let hash v = String.hash v.var
  let module_name = "Bap.Std.Var"

  let pp fmt v =
    Format.fprintf fmt "%s%s" v.var
      (if v.ver <> 0 then sprintf ".%d" v.ver else "" )
end


include T

let name v = v.var
let renumber v ver = {v with ver}
let base v = {v with ver = 0}
let typ  v = v.typ
let is_tmp v = v.tmp

let create ?(tmp=false) name typ =
  let var = if not tmp then name
    else name ^ "_" ^ Int63.to_string (Id.create ()) in
  {ver = 0; var; typ; tmp}

let version {ver} = ver
let same x y = base x = base y

module V1 = struct
  type r = string * int * typ * bool
  let serialize v = v.var, v.ver, v.typ, v.tmp
  let deserialize (var,ver,typ,tmp) = {var; ver; typ; tmp}
end

include Regular.Make(T)
