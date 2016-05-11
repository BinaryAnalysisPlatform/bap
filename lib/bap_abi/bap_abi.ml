open Core_kernel.Std
open Regular.Std
open Bap.Std

module Stack = struct
  type direction = [`up | `down]
  let create ?(direction=`down) arch =
    let module Target = (val target_of_arch arch) in
    let sz = (Arch.addr_size arch :> Size.t) in
    let width = Size.in_bits sz in
    let endian = Arch.endian arch in
    let mem = Bil.var Target.CPU.mem in
    let sp = Target.CPU.sp in
    fun off ->
      let off = Word.of_int ~width (off * Size.in_bytes sz) in
      let addr = if Word.is_zero off
        then Bil.(var sp)
        else Bil.(var sp + int off) in
      Bil.load ~mem ~addr endian sz

end
