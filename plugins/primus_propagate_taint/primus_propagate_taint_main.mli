(* When a term is indicated as a taint source, it doesn't really
   mean, that the term is the cause of the taint, but rather that
   this term operates with an object that is tainted, and we use
   this term, as a reference to this object. For example, if we know
   an address of a tainted value then we can choose a term, that
   loads a value from the given address. Or we can choose a term
   that stores the address in a register and mark this is term as a
   term that introduces the pointer taint.

   Basically, we have four kinds of expressions that may produce a
   taint:
   - a constant
   - a register
   - a load expression
   - an arbitrary expression not included in the previous
     categories.

   We can also have synthetic terms, that are introduced by analysis
   and are not used in the real program, so they should be handled
   in a special way.

   The register case is the simplest one, after evaluation of
   expression [x := y] both [x] and [y] will have the same value
   with the same id.

   The integer case is hard as every time a constant is evaluated we
   create a new value with a fresh id. Thus the only reference to
   this value will be stored in the lhs, and if it is a synthetic
   term, then this taint will not be used anywhere. However,
   analysis shouldn't introduce synthetic terms anyway (just because
   of this particular reason).

   The load expression will access a set of addresses and construct
   the final value using some sort of concatenation. We will append
   the specified taint to all the addresses that were used during
   the computation of a value. Moreover, if the constructed value is
   a pointer to a tainted object, and we have the type information,
   then we should use the size of the object and taint all addresses
   from the minimal address to the extent of the object.

   Finally, if we have arbitrary computation on the right and a
   synthetic argument on the left we are a little bit doomed, as
   every time a computation is evaluated we will have a fresh new
   value, and since the program will not refer to the synthetic
   variable we will not introduce this taint correctly. Thus we will
   choose the avenue of overtainting here and will taint values in
   all registers that were used during the computation. Given that
   there are no loads, we will have expressions like [cast:n[R0]],
   [[R1.R2]], [[R0 + 4]], etc, thus it looks like that it was an
   intention of a user to mark values used in the computation as
   references to a tainted object.
*)
