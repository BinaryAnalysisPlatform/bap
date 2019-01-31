open Core_kernel
open Bap.Std

(** maps immediates to symbols.
    For any given value, if it belongs to some basic block, then
    substitute it with [base + off], where [base] is a start of
    basic block and [off] is the offset from the [base]. *)
let resolver arch syms =
  let jump_type = match Arch.addr_size arch with
    | `r32 -> reg32_t
    | `r64 -> reg64_t in
  let make_var name =
    Bil.var (Var.create name jump_type) in
  (object inherit Bil.mapper as super
    method! map_int addr =
      Symtab.fns_of_addr syms addr |> List.hd |> function
      | Some fn ->
        let start = Block.addr (Symtab.entry_of_fn fn) in
        let sym = Symtab.name_of_fn fn in
        if Addr.(start = addr) then make_var sym else
          let off = Addr.Int_exn.(addr - start) in
          Bil.(make_var sym + int off)
      | None -> Bil.Int addr
  end)


