open Core_kernel.Std
open Regular.Std
open Bap_plugins.Std
open Format

module Std = struct

  module Tid = Type_equal.Id

  type name = ..

  type language = {
    name : name;
    repr : string;
  }

  type 'a key = 'a Tid.t

  let name_to_string = ref (fun _ -> "<language>")

  module Key = struct
    type 'a t = 'a key

    let create {repr;name} =
      name_to_string := (fun n ->
          if n = name then repr else !name_to_string n);
      Tid.create ~name:repr sexp_of_opaque

    let name = Tid.name

    include Printable.Make(struct
        type t = language
        let module_name = Some "Bap_language.Std.Language"
        let pp ppf {repr} =
          pp_print_string ppf repr
      end)

    module Name = struct
      type t = name = ..
      include Printable.Make(struct
          type t = name
          let module_name = Some "Bap_language.Std.Language.Name"
          let pp ppf n =
            pp_print_string ppf (!name_to_string n)
        end)
    end


    module type Table = sig
      type t
      type 'a data

      val create : unit -> t
      val set : t -> 'a key -> 'a data -> unit
      val get : t -> 'a key -> 'a data option
    end




    module Table(T : T1) : Table with type 'a data = 'a T.t = struct
      module U = Univ_map.Make(struct
          type 'a t = 'a T.t
          let sexp_of_t  _ = sexp_of_opaque
        end)
      type t = U.t ref
      type 'a data = 'a T.t

      let empty = U.empty

      let create () = ref empty
      let set t key data = t := U.set !t key data
      let get t key = U.find !t key
    end
  end
end
