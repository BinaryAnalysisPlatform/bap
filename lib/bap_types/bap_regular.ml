open Core_kernel.Std

module type Printable = sig
  type t
  val to_string : t -> string
  val str : unit -> t -> string
  val pps : unit -> t -> string
  val ppo : out_channel -> t -> unit
  include Pretty_printer.S     with type t := t
end

module type S = sig
  type t with bin_io, sexp, compare
  include Printable            with type t := t
  include Comparable.S_binable with type t := t
  include Hashable.S_binable   with type t := t
end

module Printable(M : sig
    include Pretty_printer.S
    val module_name : string option
  end) = struct
  include M

  let to_string t =
    Format.asprintf "%a" pp t

  let pps () t =
    to_string t

  let str = pps

  let ppo out x : unit =
    let f = Format.(formatter_of_out_channel out) in
    pp f x;
    Format.pp_print_flush f ()


  let () = Option.iter M.module_name
      ~f:(fun name -> Pretty_printer.register (name ^ ".pp"))
end

module Make(M : sig
    type t with bin_io, compare, sexp
    include Pretty_printer.S with type t := t
    val hash: t -> int
    val module_name : string option
  end) = struct
  include M
  include (Printable(M) : Printable with type t := t)
  include Comparable.Make_binable(M)
  include Hashable.Make_binable(M)
end
