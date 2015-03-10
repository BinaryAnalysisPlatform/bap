open Core_kernel.Std
open Image_backend

(* TODO: create ObjectFile.t from data and convert it to Img.t *)
let of_data (data : Bigstring.t) : Img.t option = None

let () =
  let name = "llvm_loader" in
  let r =
    Bap_image.register_backend ~name of_data in
  match r with
  | `Ok -> ()
  | `Duplicate ->
    eprintf "LLVM_backend: name «%s» is already used\n" name;
