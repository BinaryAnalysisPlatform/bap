open Core_kernel.Std

let () =
  let name = "llvm" in
  match Bap_image.register_backend ~name Llvm_loader.from_data with
  | `Ok -> ()
  | `Duplicate ->
    eprintf "llvm_loader_backend: name «%s» is already used\n" name
