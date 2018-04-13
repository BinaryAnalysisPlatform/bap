open Core_kernel.Std
open Bap.Std
open Bap_future.Std
open Bap_service


module type T = sig
  val service : service
end

module Make(T:T) = struct

  let product = ref None

  let () =
    let s = Service.request T.service |>
            Stream.parse ~init:None
              ~f:(fun p p' -> match p with
                  | None -> Some p', Some p'
                  | Some p ->
                    let p = Product.combine p p' in
                    Some p, Some p) in
    Stream.observe s (fun x -> product := Some x)
end

module Symbolizer = Make(struct
  let service = Symbolizer.service

  let provider = Provider.declare
      ~desc:"Provides a sum of all acquired symbolizers"
      "Meta-symbolizer" service
end)

module Rooter = Make(struct
  let service = Rooter.service

  let provider = Provider.declare
      ~desc:"Provides a sum of all acquired rooters"
      "Meta-rooter" service
end)

module Brancher = Make(struct
  let service = Brancher.service

  let provider = Provider.declare
      ~desc:"Provides a sum of all acquired branchers"
      "Meta-brancher" service
end)

module Recons = Make(struct
  let service = Reconstructor.service

  let provider = Provider.declare
      ~desc:"Provides a sum of all acquired reconstructors"
      "Meta-reconstructor" service
end)

module Image = Make(struct
  let service = Image.loader

  let provider = Provider.declare
      ~desc:"Provides a sum of all acquired loaders"
      "Meta-image-loader" service
end)

module Project = struct
  let service = Service.declare
      ~desc:"Combines services for delivering projects"
      ~uuid:"998638ce-6d28-4eb8-b9ea-b1d47416f7b6"
      "project"

  let provider = Provider.declare
      ~desc:"Project provider" "project" service

  let products = [
    Symbolizer.product;
    Rooter.product;
    Brancher.product;
    Recons.product;
    Image.product;
  ]

  let product () = List.fold products ~init:None
      ~f:(fun ac p ->
          match ac, !p with
          | None, None -> None
          | Some x, None
          | None, Some x -> Some x
          | Some x, Some y -> Some (Product.combine x y))
end


let digest () =
  match Project.product () with
  | None -> None
  | Some p -> Some (Product.digest p)
