open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_source_intf

module Factory(T : T) = struct
  type t = T.t
  type 'a factory = (string * ('a -> t option)) list ref
  let file   : string factory = ref []
  let binary : image factory = ref []
  let memory : (mem * arch) factory = ref []

  let add (lst : 'a factory) name x = lst := (name,x) :: !lst

  let register : type a. a source -> string -> (a -> 'b) -> unit =
    function
    | File   -> add file
    | Binary -> add binary
    | Memory -> add memory

  let enum lst = List.map ~f:fst !lst

  let list : type a. a source -> string list = function
    | File -> enum file
    | Binary -> enum binary
    | Memory -> enum memory

  let get (lst : 'a factory) name args = match List.Assoc.find !lst name with
    | None -> None
    | Some f -> f args

  let find : type a. a source -> string -> a -> 'b option = function
    | File -> get file
    | Binary -> get binary
    | Memory -> get memory
end
