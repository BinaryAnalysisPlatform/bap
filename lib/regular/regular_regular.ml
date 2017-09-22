open Core_kernel.Std
open Regular_data_types
open Regular_data_intf

module type Printable = sig
  type t
  val to_string : t -> string
  val str : unit -> t -> string
  val pps : unit -> t -> string
  val ppo : out_channel -> t -> unit
  val pp_seq : Format.formatter -> t Sequence.t -> unit
  include Pretty_printer.S     with type t := t
end

module type S = sig
  type t [@@deriving bin_io, sexp, compare]
  include Printable            with type t := t
  include Comparable.S_binable with type t := t
  include Hashable.S_binable   with type t := t
  include Data with type t := t
end

module type Minimal = sig
  (** type t should be binable, sexpable and provide compare function  *)
  type t [@@deriving bin_io, sexp, compare]
  include Pretty_printer.S with type t := t
  include Versioned.S with type t := t
  val hash : t -> int
  val module_name : string option
end

module Printable(M : sig
    include Pretty_printer.S
    val module_name : string option
  end) : Printable with type t := M.t  = struct
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

  let pp_seq ppf xs : unit =
    Regular_seq.pp pp ppf xs

  let () = Option.iter M.module_name
      ~f:(fun name ->
          Pretty_printer.register (name ^ ".pp");
          Pretty_printer.register (name ^ ".pp_seq"))
end

module Make(M : sig
    type t [@@deriving bin_io, compare, sexp]
    include Pretty_printer.S with type t := t
    include Versioned with type t := t
    val hash: t -> int
    val module_name : string option
  end) = struct
  include M
  include Comparable.Make_binable(M)
  include Hashable.Make_binable(M)

  include struct
    open Regular_data
    include Make(M)
    let bin_reader = bin_reader (module M)
    let bin_writer = bin_writer (module M)
    let sexp_reader = sexp_reader (module M)
    let sexp_writer = sexp_writer (module M)
    let printer = (Regular_data.pretty_writer (module M))

    let () =
      let ver = version in
      add_writer ~desc:"Janestreet Binary Protocol" ~ver "bin" bin_writer;
      add_reader ~desc:"Janestreet Binary Protocol" ~ver "bin" bin_reader;
      add_writer ~desc:"Janestreet Sexp Protocol" ~ver "sexp" sexp_writer;
      add_reader ~desc:"Janestreet Sexp Protocol" ~ver "sexp" sexp_reader;
      add_writer ~desc:"Pretty printer" ~ver:M.version "pretty" printer;
      set_default_printer "pretty";
      set_default_writer "bin";
      set_default_reader "bin"
  end

  include Printable(struct
      include M
      let pp ppf x = Io.print ppf x
    end)

end
