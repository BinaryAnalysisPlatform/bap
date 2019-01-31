open Core_kernel

[@@@ocaml.warning "-3"]

external init : unit -> int = "disasm_llvm_init_stub" "noalloc"

type x86_syntax = [`att | `intel] [@@deriving sexp]

module X86_syntax = struct
  type t = x86_syntax [@@deriving sexp]
  let to_string s = Sexp.to_string @@ sexp_of_t s
end

let putenv s =
  Unix.putenv
    "BAP_LLVM_OPTIONS" ("-x86-asm-syntax=" ^ X86_syntax.to_string s)

let init ?(x86_syntax=`att) () =
  let () = putenv x86_syntax in
  let r = init () in
  if r < 0 then
    Error (Error.of_string (sprintf "LLVM initialization failed with error %d" r))
  else Ok ()

let version = Bap_llvm_config.version
