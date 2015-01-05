open Core_kernel.Std
open Bap.Std

type request
type response
type target
type id with sexp_of
type links = Uri.t List1.t
type 'a resource = links * 'a
type res_id = string
type res_ids = res_id list with sexp_of


module Id : Identifiable with type t := id


module Target : sig
  type t = target

  val arm :  Disasm.Arm.Insn.t -> Disasm.Arm.Op.t list -> t

end

module Request : sig
  type t = request

  val id : t -> id Or_error.t

  val accept : t ->
    init:(string -> 'a) ->
    load_file:(?loader:string -> Uri.t -> 'a) ->
    load_chunk:(addr -> arch -> endian -> Uri.t -> 'a) ->
    get_insns:(?backend:res_id -> Disasm.Basic.pred list -> res_id -> 'a) ->
    get_resource:(res_id -> 'a) -> 'a Or_error.t
end

module Response : sig
  type t = response
  type msg
  type insn
  type loader
  type disassembler
  type transport


  (** creates a response to the request with the [id]  *)
  val create : id -> msg -> t

  val error : [`Critical | `Error | `Warning] -> string -> msg

  val capabilities : version:string ->
    transport list -> loader list -> disassembler list -> msg

  val image : secs:res_ids -> Image.t resource -> msg

  val section : syms:res_ids -> Section.t -> mem resource -> msg

  val symbol : Symbol.t -> mem resource List1.t -> msg

  val memory : mem resource -> msg

  val insn : ?target:target -> ?bil:stmt list ->
    mem resource -> Disasm.Basic.full_insn -> insn

  val insns : insn list -> msg

  val images   : res_id list -> msg
  val sections : res_id list -> msg
  val symbols  : res_id list -> msg
  val chunks   : res_id list -> msg
  val added    : res_id -> msg

  val loader : name:string -> arch:arch -> format:string ->
    [`symtab | `debug] list -> loader

  val disassembler : name:string -> arch:arch ->
    kinds:Disasm.kind list -> has_name:bool -> has_ops:bool ->
    has_target:bool -> has_bil:bool -> disassembler

  val transport : string -> transport

end
