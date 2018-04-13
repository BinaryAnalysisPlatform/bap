open Core_kernel.Std
open Bap.Std
open Bap_future.Std
open Bap_service

let combine_products service ~f =
  let s = Service.request service |>
          Stream.parse ~init:None
            ~f:(fun p p' -> match p with
                | None -> Some p', Some p'
                | Some p ->
                  let p = Product.combine p p' in
                  Some p, Some p) in
  Stream.observe s f

module Symbs = struct
  let service = Symbolizer.service

  let provider = Provider.declare
      ~desc:"Provides a sum of all acquired symbolizers"
      "Meta-symbolizer" service

  let product = ref None

  let () = combine_products service ~f:(fun x -> product := Some x)
end

module Roots = struct
  let service = Rooter.service

  let provider = Provider.declare
      ~desc:"Provides a sum of all acquired rooters"
      "Meta-rooter" service

  let product = ref None

  let () = combine_products service ~f:(fun x -> product := Some x)
end

module Branchs = struct
  let service = Brancher.service

  let provider = Provider.declare
      ~desc:"Provides a sum of all acquired branchers"
      "Meta-brancher" service

  let product = ref None

  let () = combine_products service ~f:(fun x -> product := Some x)
end

module Recons = struct
  let service = Reconstructor.service

  let provider = Provider.declare
      ~desc:"Provides a sum of all acquired reconstructors"
      "Meta-reconstructor" service

  let product = ref None

  let () = combine_products service ~f:(fun x -> product := Some x)
end

module Images = struct
  let service = Image.loader

  let provider = Provider.declare
      ~desc:"Provides a sum of all acquired loaders"
      "Meta-image-loader" service

  let product = ref None

  let () = combine_products service ~f:(fun x -> product := Some x)
end

let service = Service.declare
    ~desc:"Combines services for delivering projects"
    ~uuid:"998638ce-6d28-4eb8-b9ea-b1d47416f7b6"
    "project"

let provider = Provider.declare
    ~desc:"Project provider" "project" service

let products = [
  Symbs.product;
  Roots.product;
  Branchs.product;
  Recons.product;
  Images.product;
]

let product () = List.fold products ~init:None
    ~f:(fun ac p ->
        match ac, !p with
        | None, None -> None
        | Some x, None
        | None, Some x -> Some x
        | Some x, Some y -> Some (Product.combine x y))

let digest () =
  match product () with
  | None -> None
  | Some p -> Some (Product.digest p)
