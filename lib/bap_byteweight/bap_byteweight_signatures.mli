(** Provides signatures storage  *)
open Bap.Std


val save : ?comp:string -> mode:string -> path:string -> arch -> string -> unit
val load : ?comp:string -> ?path:string -> mode:string -> arch -> string option
val default_path : string
