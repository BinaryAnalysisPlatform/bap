open Core_kernel.Std

module Doc = struct
  type t = Ogre.Doc.t [@@deriving compare]

  module Stringable = struct
    type t = Ogre.doc
    let to_string = Ogre.Doc.to_string
    let of_string data = match Ogre.Doc.from_string data with
      | Ok doc -> doc
      | Error err ->
        failwithf "can't deserialize Ogre doc - %s"
          (Error.to_string_hum err) ()
  end

  let pp = Ogre.Doc.pp
  include Sexpable.Of_stringable(Stringable)
  include Binable.Of_stringable(Stringable)
end
