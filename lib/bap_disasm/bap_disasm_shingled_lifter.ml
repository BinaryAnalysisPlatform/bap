(* Uses shingled to get list of insns for every offset *)
open Core_kernel.Std
open Or_error
open Bap_types.Std

module Dis = Bap_disasm_basic
module Insn = Bap_disasm_insn
module Memory = Bap_memory
module Arch= Bap_arch
module Memmap = Bap_memmap
exception Create_mem of Error.t

let create_memory arch s addr =
  let endian = Arch.endian arch in
  Memory.create endian addr @@
  Bigstring.of_string s |> function
  | Ok r -> r
  | Error e -> raise (Create_mem e)

let lift_all ?backend ?min_addr arch string_mem =
  let addr_size= Size.to_bits @@ Arch.addr_size arch in
  let min_addr =
    Option.value min_addr ~default:(Addr.of_int addr_size 0) in
  let backend = Option.value backend ~default:"llvm" in
  let gmem = (create_memory arch string_mem min_addr) in
  let dis = Dis.create ~backend (Arch.to_string arch) |> ok_exn in
  let open Seq.Generator in
  Bap_disasm_shingled.all_shingles dis gmem ~init:[]
    ~at:(fun accu (mem,insn) -> match insn with
        | Some insn -> Insn.(bil @@ of_basic insn) ::accu
        | None -> accu)


let lift_sheered ?backend ?min_addr arch string_mem =
  let addr_size= Size.to_bits @@ Arch.addr_size arch in
  let min_addr =
    Option.value min_addr ~default:(Addr.of_int addr_size 0) in
  let gmem = (create_memory arch string_mem min_addr) in
  let shingles = Bap_disasm_shingled.sheered_shingles
      ?backend arch gmem in
  Memmap.map shingles ~f:(fun insn ->
      Insn.(bil insn))
    
