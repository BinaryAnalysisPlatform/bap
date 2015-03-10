open Core_kernel.Std

let () =
  let name = "llvm_loader" in
  let r =
    Bap_image.register_backend ~name Llvm_loader.from_data in
  match r with
  | `Ok -> ()
  | `Duplicate ->
    eprintf "llvm_loader_backend: name «%s» is already used\n" name;
