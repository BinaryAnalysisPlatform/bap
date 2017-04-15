open Core_kernel.Std
open Bap_types.Std

type img = Image_backend.Img.t
type doc = Ogre.doc

module Make(M : Monads.Std.Monad.S) : sig
  val doc_of_image : img -> doc Or_error.t M.t
  val image_of_doc : doc -> img Or_error.t M.t
end
