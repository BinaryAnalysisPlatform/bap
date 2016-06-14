open Format

module Create() : sig
  val name : string
  val version : string
  val doc : string
  val argv : string array

  val debug   : ('a,formatter,unit) format -> 'a
  val info    : ('a,formatter,unit) format -> 'a
  val warning : ('a,formatter,unit) format -> 'a
  val error   : ('a,formatter,unit) format -> 'a

  module Config : sig
    (** Version number  *)
    val version : string

    (** A directory for bap specific read-only architecture
        independent data files.  *)
    val datadir : string

    (** A directory for bap specific object files, libraries, and
        internal binaries that are not intended to be executed directly
        by users or shell scripts *)
    val libdir : string

    (** A directory for bap specific configuaration files  *)
    val confdir : string

    (** Get configuration value for a plugin that calls it *)
    val get : string -> string option

    (** Set configuration value for a plugin that calls it *)
    val set : name:string -> data:string -> unit

    (** List all known configuration values for a plugin that calls it *)
    val options : unit -> (string * string) list
  end

end
