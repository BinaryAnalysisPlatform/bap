#!/usr/bin/env python

"""Disassembled instuctions"""

from adt import ADT

class Kind(ADT) : pass
class Having_side_effects(Kind)    : pass
class Affecting_control(Kind)      : pass
class Branch(Affecting_control)    : pass
class Conditional_branch(Branch)    : pass
class Unconditional_branch(Branch)  : pass
class Indirect_branch(Branch)       : pass
class Return(Affecting_control)    : pass
class Call(Affecting_control)      : pass
class Barrier(Affecting_control)   : pass
class Terminator(Affecting_control): pass
class May_affect_control_flow(Affecting_control) : pass
class May_load(Having_side_effects)     : pass
class May_store(Having_side_effects)    : pass


def eval_if_not_adt(s):
    if isinstance(s, ADT):
        return s
    else:
        return eval(s)


def map_eval(ss):
    return [eval_if_not_adt(s) for s in ss]



class Insn(object) :
    def __init__(self, name, addr, size, asm, kinds, operands, target=None, bil=[], **kw):
        self.name  = name
        self.addr  = int(addr)
        self.size  = int(size)
        self.operands = map_eval(operands)
        self.asm   = str(asm)
        self.kinds = map_eval(kinds)
        self.target = target
        self.bil = bil
        self.__dict__.update(kw)

    def __repr__(self):
        return 'Insn("{name}", {addr:#010x}, {size}, "{asm}", {kinds}, {operands}, {target}, {bil})'.\
          format(**self.__dict__)

class Op(ADT)        : pass
class Reg(Op)        : pass
class Imm(Op)        : pass
class Fmm(Op)        : pass


if __name__ == "__main__":
    print Reg('R0')
    for insn in ["Reg(\"R0\")", "Imm(5)", "Imm(14)", "Reg(\"Nil\")", "Reg(\"Nil\")"]:
        print eval(insn)
