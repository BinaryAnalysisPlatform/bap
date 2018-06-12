open Core_kernel
open Bap.Std
open Regular.Std

type memo = {
    addr : addr;
    code : word;
  } [@@deriving bin_io, compare, sexp]

module Memo = struct
  type t = memo [@@deriving bin_io, compare, sexp]

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]

      let hash = Hashtbl.hash
      let module_name = Some "Bil_memo.Memo"
      let version = "1.0.0"
      let pp fmt {addr; code} =
        Format.fprintf fmt "%a%a" Addr.pp addr Word.pp code
    end)
end

let insns : bil Memo.Table.t = Memo.Table.create ()

let find addr code _ =
  match Hashtbl.find insns {addr; code} with
  | None -> Error (Error.of_string "bil not found")
  | Some bil -> Ok bil

let save addr code bil =
  Hashtbl.update insns {addr; code} ~f:(fun _ -> bil);
  Ok bil