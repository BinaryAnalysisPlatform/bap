open Core_kernel
open Regular_data_intf
open Format


type key = string

type 'a t = {
  load : string -> 'a option;
  save : string -> 'a -> unit;
} [@@deriving fields]

let create ~load ~save = {load; save}

type service = {
  create : 'a . 'a reader -> 'a writer -> 'a t
}

module Service = struct
  let oblivion = {
    create = fun _ _ -> {
        load = (fun _ -> None);
        save = (fun _ _ -> ());
      }
  }

  let service = ref oblivion

  let provide new_service = service := new_service
  let request x y = !service.create x y
end



module Digest = struct
  include String

  let make s = s |> Digest.string |> Digest.to_hex

  let format fmt =
    let buf = Buffer.create 4096 in
    let ppf = formatter_of_buffer buf in
    let key ppf =
      pp_print_flush ppf ();
      Buffer.contents buf |> make in
    kfprintf key ppf fmt

  let add buf fmt = format ("%s"^^fmt) buf
  let add_sexp d sexp_of x = add d "%a" Sexp.pp (sexp_of x)
  let add_file d name = add d "%s" (Digest.file name)
  let create ~namespace = make namespace
end

let digest ~namespace fmt =
  Digest.format ("%s"^^fmt) namespace
