open Core_kernel.Std
open Regular.Std
open Bap.Std
open Format

type perm = [`code | `data] [@@deriving sexp]
type section = string * perm * int * (int64 * int)
  [@@deriving sexp]

type image = string * addr_size * section list [@@deriving sexp]

module Img = struct
  type t = image

  include Data.Make(struct
      type t = image
      let version = "0.1"
    end)
end

module Symbols = struct
  type t = (string * int64 * int64) list

  include Data.Make(struct
      type t = (string * int64 * int64) list
      let version = "0.1"
    end)
end

module Brancher_info = struct
  type t = (int64 * int64 option * int64 list) list

  include Data.Make(struct
      type t = (int64 * int64 option * int64 list) list
      let version = "0.1"
    end)

  let pp ppf x =
    List.iter x ~f:(fun (place,dest,rest) ->
        let s2 = List.fold ~init:" " rest ~f:(fun acc x ->
            let s = sprintf "0x%x" @@ Int64.to_int_exn x in
            " "^s^acc) in
        let s0 = sprintf "0x%x" @@ Int64.to_int_exn place in
        let s1 = match dest with
          | Some dest -> sprintf "0x%x" @@ Int64.to_int_exn dest
          | None -> "" in
        printf "%s (%s) (%s)@." s0 s1 s2)
end
