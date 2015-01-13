#!/usr/bin/env python
"""
Algebraic Data Types (ADT) is used to represent two kinds of things:

1. A discrimintated union of types, called sum
2. A combination of some types, called product.

# Sum types

Sum types represents a concept of generalizing. For example,
on ARM R0 and R1 are all general purpose registers (GPR). Also on ARM
we have Condition Code registers (CCR) :

   class Reg(ADT) : pass
   class GPR(Reg) : pass
   class CCR(Reg) : pass
   class R0(GPR)  : pass
   class R1(GPR)  : pass


That states that a register can be either R0 or R1, but not both.

# Product types

Product types represent a combination of other types. For example,
mov instruction has two arguments, and the arguments are also ADT's by
itself:

   def Insn(ADT) : pass
   def Mov(Insn) : pass

   Mov(R0(), R1())


# Comparison

ADT objects are compared structurally: if they have the same class and
and their values are structurally the same, then they are equal, i.e.,

   assert(R0() == R0())
   assert(R1() != R0())

"""

from collections import Iterable

class ADT(object):
    """ Algebraic Data Type.

    This is a base class for all ADTs. ADT represented by a tuple of arguments,
    stored in a val field. Arguments should be instances of ADT class, or numbers,
    or strings. Empty set of arguments is permitted.
    A one-tuple is automatically untupled, i.e., `Int(12)` has value `12`, not `(12,)`.
    For convenience, a name of the constructor is provided in `name` field.

    A structural comparison is provided.

    """
    def __init__(self, *args):
        self.constr = self.__class__.__name__
        self.arg = args if len(args) != 1 else args[0]

    def __cmp__(self,other):
        return self.__dict__.__cmp__(other.__dict__)

    def __repr__(self):
        def qstr(x):
            if isinstance(x, (int,long)):
                return '0x{0:x}'.format(x)
            elif isinstance(x, ADT):
                return str(x)
            else:
                return '"{0}"'.format(x)
        def args():
            if isinstance(self.arg, tuple):
                return ", ".join(qstr(x) for x in self.arg)
            else:
                return qstr(self.arg)

        return "{0}({1})".format(self.constr, args())


class Visitor(object):
    """ ADT Visitor.
    This class helps to perform iterations over arbitrary ADTs.

    This visitor supports, subtyping, i.e. you can match not only on
    leaf constructors, but also on their bases. For example, with
    the `Exp` hierarchy, provided below, you can visit all binary operators,
    by overriding `visit_BinOp` method. See `run` method description for
    more infromation.
    """

    def visit_ADT(self, adt):
        """Default visitor.

        This method will be called for those data types that has
        no specific visitors. It will recursively descent into all
        ADT values.
        """
        if isinstance(adt.arg, tuple):
            for e in adt.arg:
                self.run(e)
        elif isinstance(adt.arg, ADT):
            self.run(adt.arg)


    def run(self, adt):
        """ADT.run(adt-or-iterable) -> None

        if adt is iterable, the run is called recursively for each member
        of adt.

        Otherwise, for an ADT of type C the method `visit_C` is looked up in the
        visitors methods dictionary. If it doesn't exist, then `visit_B` is
        looked up, where `D` is the base class of `C`. The process continues,
        until the method is found. This is guaranteed to terminate,
        since visit_ADT method is defined.

        Note: Non ADTs will be silently ignored.

        Once the method is found it is called. It is the method's responsiblity
        to recurse into sub-elements, e.g., call run method.

        For example, suppose that we want to count negative values in a given
        BIL expression:

        class CountNegatives(Visitor):
            def __init__(self):
                self.neg = False
                self.count = 0

            def visit_Int(self, int):
                if int.arg < 0 and not self.neg \
                  or int.arg > 0 and self.neg:
                    self.count += 1

            def visit_NEG(self, op):
                was = self.neg
                self.neg = not was
                self.run(op.arg)
                self.neg = was

        We need to keep track on the unary negation operator, and, of
        course, we need to look for immediates, so we override two methods:
        visit_Int for Int constructor and visit_NEG for counting unary minuses.
        (Actually we should count for bitwise NOT operation also, since it will
        change the sign bit also, but lets forget about it for the matter of the
        excercise (and it can be easily fixed just by matching visit_UnOp)).

        When we hit visit_NEG we toggle current sign, storing its previous value
        and recurse into the operand. After we return from the recursion, we restore
        the sign.
        """
        if isinstance(adt, ADT):
            for c in adt.__class__.mro():
                name = ("visit_%s" % c.__name__)
                fn = getattr(self, name, None)
                if fn is not None:
                    return fn(adt)


def visit(visitor, adt):

    if isinstance(adt, Iterable):
        for x in adt:
            visitor.run(x)
    else:
        visitor.run(adt)
    return visitor


if __name__ == "__main__":
    class Fruit(ADT) : pass
    class Bannana(Fruit) : pass
    class Apple(Fruit) : pass

    assert(Bannana() == Bannana())
    assert(Bannana() != Apple())
    assert(  Apple() <  Bannana())
