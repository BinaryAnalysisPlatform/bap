#!/usr/bin/env python

"""BAP BIL Python representation"""

from adt import *


class Exp(ADT)  : pass     # Abstract base for all expressions
class Load(Exp):
    "Load(mem,idx,endian,size)"
    @property
    def mem(self) : return self.arg[0]
    @property
    def idx(self) : return self.arg[1]
    @property
    def endian(self): return self.arg[2]
    @property
    def size(self): return self.arg[3]


class Store(Exp):
    "Store(mem,idx,val,endian,size"
    @property
    def mem(self) : return self.arg[0]
    @property
    def idx(self) : return self.arg[1]
    @property
    def value(self): return self.arg[2]
    @property
    def endian(self): return self.arg[3]
    @property
    def size(self): return self.arg[4]

class BinOp(Exp):
    "Abstract base for all binary operators"
    @property
    def lhs(self): return self.arg[0]
    @property
    def rhs(self): return self.arg[1]

class UnOp(Exp) : pass     # Abstract base for all unary operators

class Var(Exp)  :
    "Var(name,type)"
    @property
    def name(self): return self.arg[0]
    @property
    def type(self): return self.arg[1]

class Int(Exp):
    "Int(int,size)"
    @property
    def value(self): return self.arg[0]
    @property
    def size(self):
        "word size in bits"
        return self.arg[1]

class Cast(Exp) :
    "Abstract base for all cast operations"
    @property
    def size(self): return self.arg[0]
    @property
    def expr(self): return self.arg[1]

class Let(Exp)  :
    "Let(var,val,expr)"
    @property
    def var(self): return self.arg[0]
    @property
    def value(self): return self.arg[1]
    @property
    def expr(self): return self.arg[2]

class Unknown(Exp):
    "Unknown(string,type)"
    @property
    def desc(self): return self.arg[0]
    @property
    def type(self): return self.arg[1]

class Ite(Exp):
    "Ite (cond,if_true,if_false)"
    @property
    def cond(self): return self.arg[0]
    @property
    def true(self): return self.arg[1]
    @property
    def false(self): return self.arg[2]

class Extract(Exp):
    "Extract(hb,lb, exp)"
    @property
    def high_bit(self): return self.arg[0]
    @property
    def low_bit(self): return self.arg[1]
    @property
    def expr(self): return self.arg[2]

class Concat(Exp):
    @property
    def lhs(self): return self.arg[0]
    @property
    def rhs(self): return self.arg[1]

class Stmt(ADT) : pass     # Abstract base for all statements

class Move(Stmt) :
    "Move(var,exp)"
    @property
    def var(self): return self.arg[0]
    @property
    def expr(self): return self.arg[1]

class Jmp(Stmt) : pass     # Jmp(exp)
class Special(Stmt): pass  # Special (string)
class While(Stmt) :
    "While (cond, stmts)"
    @property
    def cond(self): return self.arg[0]

    @property
    def stmts(self): return self.arg[1]

class If(Stmt) :
    "If(cond, yes-exprs, no-exprs)"
    @property
    def cond(self): return self.arg[0]
    @property
    def true(self): return self.arg[1]
    @property
    def false(self): return self.arg[2]

class CpuExn(Stmt) : pass  # CpuExn(n)

# All BinOps have two operands of type exp
class PLUS    (BinOp) : pass
class MINUS   (BinOp) : pass
class TIMES   (BinOp) : pass
class DIVIDE  (BinOp) : pass
class SDIVIDE (BinOp) : pass
class MOD     (BinOp) : pass
class SMOD    (BinOp) : pass
class LSHIFT  (BinOp) : pass
class RSHIFT  (BinOp) : pass
class ARSHIFT (BinOp) : pass
class AND     (BinOp) : pass
class OR      (BinOp) : pass
class XOR     (BinOp) : pass
class EQ      (BinOp) : pass
class NEQ     (BinOp) : pass
class LT      (BinOp) : pass
class LE      (BinOp) : pass
class SLT     (BinOp) : pass
class SLE     (BinOp) : pass

# All UnOps have one operand of type exp
class NEG     (UnOp)  : pass
class NOT     (UnOp)  : pass

# All Casts have two operands: (Int(size),exp)
class UNSIGNED(Cast)  : pass
class SIGNED(Cast)    : pass
class HIGH(Cast)      : pass
class LOW(Cast)       : pass

# Endians doesn't have values
class Endian(ADT) : pass
class LittleEndian(Endian) : pass
class BigEndian(Endian) : pass

class Type(ADT) : pass  # Abstract base for expression type
class Imm(Type) :
    "Imm(size) - immediate value"
    @property
    def size(self): return self.arg[0]

class Mem(Type) :
    "Mem(addr_size, value_size)"
    @property
    def addr_size(self): return self.arg[0]

    @property
    def value_size(self): return self.arg[1]

def loads(s):
    return eval(s)

# A playground.

if __name__ == "__main__":

    exp = Load(Int(12,32),Int(14,32), LittleEndian())
    print exp
    exp = Load(exp, exp, BigEndian())


    class CountEvens(Visitor):
        def __init__(self):
            self.count = 0


        def visit_Int(self, int):
            self.count += 1

    class CountNegatives(Visitor):
        def __init__(self):
            self.neg = False
            self.count = 0

        def visit_Int(self, int):
            if int.value < 0 and not self.neg \
              or int.value > 0 and self.neg:
                self.count += 1

        def visit_MINUS(self, op):
            self.run(op.lhs)
            was = self.neg
            self.neg = not was
            self.run(op.rhs)
            self.neg = was

        def visit_NEG(self, op):
            was = self.neg
            self.neg = not was
            self.run(op.arg)
            self.neg = was

    print "%s" % exp
    counter = CountEvens()
    counter.run(exp)
    print counter.count
    exp = eval("%s" % exp)
    print "%s" % exp
    counter = CountEvens()
    counter.run(exp)
    print counter.count

    minus_one = NEG(NEG(Int(-1,32)))
    zero = MINUS(minus_one, minus_one)
    print zero

    nc = CountNegatives()
    nc.run(zero)
    print nc.count
