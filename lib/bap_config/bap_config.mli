val get : string -> string option

val set : name:string -> data:string -> unit

val options : unit -> (string * string) list

(* Some ./configure time constants *)
val name : string
val version : string
val os_type : string
val system : string
val architecture : string
val ocaml_version : string
val prefix : string
val exec_prefix : string
val bindir : string
val sbindir : string
val libexecdir : string
val confdir : string
val sharedstatedir : string
val localstatedir : string
val libdir : string
val datarootdir : string
val datadir : string
val infodir : string
val localedir : string
val mandir : string
val docdir : string
val htmldir : string
val dvidir : string
val pdfdir : string
val psdir : string
val suffix_program : string
val debug : string
val profile : string
val standard_library : string
