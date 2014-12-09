#!/usr/bin/env python


from collections import Iterable

class ADT(object):
    """ Algebraic Data Type.

    This is a base class for all ADTs. ADT represented by a tuple of arguments
    If a particular constructor doesn't have an assosiated
    value then it is an empty tuple. A one-tuple is automatically unboxed,
    i.e., `Int(12)` has value `12`, not `(12,)`.
    For convinience, a name of the constructor is provided in `name` field.
    """
    def __init__(self, val=()):
        self.val = val
        self.name = self.__class__.__name__

    def __repr__(self):
        if self.val is ():
            return "%s()" % self.name
        else:
            return "%s(%r)" % (self.name, self.val)

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
        if isinstance(adt.val, Iterable):
            for e in adt.val:
                self.run(e)

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
        expression:

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
        if isinstance(adt, Iterable):
            for s in adt:
                self.run(s)
        if isinstance(adt, ADT):
            for c in adt.__class__.mro():
                name = ("visit_%s" % c.__name__)
                fn = getattr(self, name, None)
                if fn is not None:
                    return fn(adt)
