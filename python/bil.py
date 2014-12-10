#!/usr/bin/env python

"""BAP BIL Python representation"""

from adt import ADT, Visitor


class Exp(ADT)  : pass     # Abstract base for all expressions
class Load(Exp) : pass     # Load((mem,idx,endian,Int(size)))
class Store(Exp): pass     # Store((mem,idx,val,endian,Int(size)))
class BinOp(Exp): pass     # Abstract base for all binary operators
class UnOp(Exp) : pass     # Abstract base for all unary operators
class Var(Exp)  : pass     # Var((name,type))
class Int(Exp)  : pass     # Int(int)
class Cast(Exp) : pass     # Abstract base for all cast operations
class Let(Exp)  : pass     # Let((var,val,body))
class Unknown(Exp): pass   # Unknown(string,type)
class Ite(Exp): pass       # Ite ((cond,if_true,if_false))
class Extract(Exp): pass   # Extract(Int(hb),Int(lb), exp)
class Concat(Exp): pass    # Concat((lhs,rhs))

class Stmt(ADT) : pass     # Abstract base for all statements

class Move(Stmt) : pass    # Move((var,exp))
class Jmp(Stmt) : pass     # Jmp(exp)
class Special(Stmt): pass  # Special (string)
class While(Stmt) : pass   # While ((cond, exps))
class If(Stmt) : pass      # If((cond, yes-exprs, no-exprs))
class CpuExn(Stmt) : pass  # CpuExn(Int(n))

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
class Imm(Type) : pass  # Imm(Int(size)) - immediate value
class Mem(Type) : pass  # Mem(Int(addr_size), Int(value_size))


# A playground.

if __name__ == "__main__":

    exp = Load(Int(12),Int(14), LittleEndian())
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
            if int.val < 0 and not self.neg \
              or int.val > 0 and self.neg:
                self.count += 1

        def visit_NEG(self, op):
            was = self.neg
            self.neg = not was
            self.run(op.val)
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

    zero = PLUS((NEG(NEG(Int(-1))), NEG(NEG(Int(1)))))
    print zero

    nc = CountNegatives()
    nc.run(zero)
    print nc.count
