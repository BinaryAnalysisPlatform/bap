#!/usr/bin/env python

"""Disassembled instuctions"""

from adt import ADT

class Kind(ADT) : pass
class Having_side_effects(Kind)    : pass
class Affecting_control(Kind)      : pass
class Branch(Affecting_control)    : pass
class ConditionalBranch(Branch)    : pass
class UnconditionalBranch(Branch)  : pass
class IndirectBranch(Branch)       : pass
class Return(Affecting_control)    : pass
class Call(Affecting_control)      : pass
class Barrier(Affecting_control)   : pass
class Terminator(Affecting_control): pass
class May_affect_control_flow(Affecting_control) : pass
class May_load(Having_side_effects)     : pass
class May_store(Having_side_effects)    : pass

class Insn(object) :
    def __init__(self, name, addr, size, asm, kinds, ops):
        self.name  = name
        self.addr  = addr
        self.size  = size
        self.ops   = ops
        self.asm   = asm
        self.kinds = set(kinds)
        print self.__dict__

    def __repr__(self):
        return "Insn({name}, {addr}, {size}, '{asm}', {kinds}, {ops})".\
          format(**self.__dict__)

class Op(ADT)        : pass
class Reg(Op)        : pass
class Imm(Op)        : pass
class Fmm(Op)        : pass


if __name__ == "__main__":
    insn = eval("Insn('MOVi', 0x40000, 0x4, 'mov r0, #5', [Call()], (Reg('R0'), Imm(5), Imm(14), Reg('nil'), Reg('nil')))")
    print insn
