open Bap_types.Std
open Bap_image_std
open Core_kernel.Std

let img_of_filename filename = 
  let img, errs = Image.create filename |> ok_exn in
  List.iter errs ~f:(fun err ->
      (Error.pp Format.std_formatter err);
    );
  img

let create_memory arch min_addr data =
  let data = Bigstring.of_string data in
  Memory.create (Arch.endian arch) min_addr data
