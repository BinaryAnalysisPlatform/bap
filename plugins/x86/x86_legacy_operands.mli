(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)

module Dis = Bap.Std.Disasm_expert.Basic
module Insn = Dis.Insn
val r :
  f:('a ->
     Bap.Std.reg -> ('b, Core_kernel.Error.t) Core_kernel.Result.t) ->
  'a ->
  (Dis.asm, Dis.kinds) Insn.t ->
  ('b, Core_kernel.Error.t) Core_kernel.Result.t
val i :
  f:('a ->
     Bap.Std.imm -> ('b, Core_kernel.Error.t) Core_kernel.Result.t) ->
  'a ->
  (Dis.asm, Dis.kinds) Insn.t ->
  ('b, Core_kernel.Error.t) Core_kernel.Result.t
val m :
  f:('a ->
     seg:Bap.Std.reg ->
     base:Bap.Std.reg ->
     scale:Bap.Std.imm ->
     index:Bap.Std.reg ->
     disp:Bap.Std.imm ->
     ('b, Core_kernel.Error.t) Core_kernel.Result.t) ->
  'a ->
  (Dis.asm, Dis.kinds) Insn.t ->
  ('b, Core_kernel.Error.t) Core_kernel.Result.t
val rr :
  f:('a ->
     Bap.Std.reg ->
     Bap.Std.reg -> ('b, Core_kernel.Error.t) Core_kernel.Result.t) ->
  'a ->
  (Dis.asm, Dis.kinds) Insn.t ->
  ('b, Core_kernel.Error.t) Core_kernel.Result.t
val ri :
  f:('a ->
     Bap.Std.reg ->
     Bap.Std.imm -> ('b, Core_kernel.Error.t) Core_kernel.Result.t) ->
  'a ->
  (Dis.asm, Dis.kinds) Insn.t ->
  ('b, Core_kernel.Error.t) Core_kernel.Result.t
val ir :
  f:('a ->
     Bap.Std.imm ->
     Bap.Std.reg -> ('b, Core_kernel.Error.t) Core_kernel.Result.t) ->
  'a ->
  (Dis.asm, Dis.kinds) Insn.t ->
  ('b, Core_kernel.Error.t) Core_kernel.Result.t
val rm :
  f:('a ->
     Bap.Std.reg ->
     seg:Bap.Std.reg ->
     base:Bap.Std.reg ->
     scale:Bap.Std.imm ->
     index:Bap.Std.reg ->
     disp:Bap.Std.imm ->
     ('b, Core_kernel.Error.t) Core_kernel.Result.t) ->
  'a ->
  (Dis.asm, Dis.kinds) Insn.t ->
  ('b, Core_kernel.Error.t) Core_kernel.Result.t
val mr :
  f:('a ->
     seg:Bap.Std.reg ->
     base:Bap.Std.reg ->
     scale:Bap.Std.imm ->
     index:Bap.Std.reg ->
     disp:Bap.Std.imm ->
     Bap.Std.reg -> ('b, Core_kernel.Error.t) Core_kernel.Result.t) ->
  'a ->
  (Dis.asm, Dis.kinds) Insn.t ->
  ('b, Core_kernel.Error.t) Core_kernel.Result.t
val mi :
  f:('a ->
     seg:Bap.Std.reg ->
     base:Bap.Std.reg ->
     scale:Bap.Std.imm ->
     index:Bap.Std.reg ->
     disp:Bap.Std.imm ->
     Bap.Std.imm -> ('b, Core_kernel.Error.t) Core_kernel.Result.t) ->
  'a ->
  (Dis.asm, Dis.kinds) Insn.t ->
  ('b, Core_kernel.Error.t) Core_kernel.Result.t
val rrr :
  f:('a ->
     Bap.Std.reg ->
     Bap.Std.reg ->
     Bap.Std.reg -> ('b, Core_kernel.Error.t) Core_kernel.Result.t) ->
  'a ->
  (Dis.asm, Dis.kinds) Insn.t ->
  ('b, Core_kernel.Error.t) Core_kernel.Result.t
val rri :
  f:('a ->
     Bap.Std.reg ->
     Bap.Std.reg ->
     Bap.Std.imm -> ('b, Core_kernel.Error.t) Core_kernel.Result.t) ->
  'a ->
  (Dis.asm, Dis.kinds) Insn.t ->
  ('b, Core_kernel.Error.t) Core_kernel.Result.t
val rrm :
  f:('a ->
     Bap.Std.reg ->
     Bap.Std.reg ->
     seg:Bap.Std.reg ->
     base:Bap.Std.reg ->
     scale:Bap.Std.imm ->
     index:Bap.Std.reg ->
     disp:Bap.Std.imm ->
     ('b, Core_kernel.Error.t) Core_kernel.Result.t) ->
  'a ->
  (Dis.asm, Dis.kinds) Insn.t ->
  ('b, Core_kernel.Error.t) Core_kernel.Result.t
val rrri :
  f:('a ->
     Bap.Std.reg ->
     Bap.Std.reg ->
     Bap.Std.reg ->
     Bap.Std.imm -> ('b, Core_kernel.Error.t) Core_kernel.Result.t) ->
  'a ->
  (Dis.asm, Dis.kinds) Insn.t ->
  ('b, Core_kernel.Error.t) Core_kernel.Result.t
