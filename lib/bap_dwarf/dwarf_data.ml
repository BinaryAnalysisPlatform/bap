open Core_kernel.Std
open Bap.Std

open Dwarf_types

module Buffer = struct
  type 'a t = {
    data : 'a;
    pos : int;
  } with sexp,bin_io,compare,fields

  let with_pos t pos = { t with pos }
  let with_off t off = { t with pos = t.pos + off}

  let create ?(pos=0) data = { data; pos }
end

type 'a buffer = 'a Buffer.t


module Section = struct
  module T = struct
    type t = Section.t with sexp,bin_io,compare
  end
  include T
  include Comparable.Make(T)
  include Sexpable.To_stringable(T)
end


type 'a t = {
  sections : 'a buffer Section.Map.t;
  endian : endian;
}

let create endian ss : 'a t Or_error.t =
  let open Or_error.Monad_infix in
  Section.Map.of_alist_or_error ss >>|
  fun sections -> {sections; endian}


let section t section =
  match Section.Map.find t.sections section with
  | Some buf -> Ok buf
  | None ->
    Or_error.errorf "section %s is not available" @@
    Section.to_string section

let endian d = d.endian
