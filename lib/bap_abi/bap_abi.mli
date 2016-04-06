open Bap.Std
(** ABI dispatching interface.

    ABI tranfers a high-level API for the given language into a
    low-level representation. For each language there is a set of
    targets, supported by the language, and each target has a set of
    ABIs. For example, [C] language has [`arm], [`x86], [`x86-64]
    targets, and [`x86] target supports [cdecl], [stdcall], [fastcall]
    etc ABIs.

    The amount of work, performed by each ABI is not constrained, but
    usually their task is to insert [arg terms] in stub functions,
    based on the provided API.

    For each particular function the following method is used to apply
    an ABI:
    1. if [resolve data sub = None] then apply [default] abi;
    2. if [resolve data sub = Some abi], then apply [abi];
    3. if an abi to be applied is not known, then don't touch the function.

    The resolver [resolve] can be overwritten. It defaults to a
    function, that returns [None] for any input. The resolver
    indirection is useful, for languages, that allows to change ABI
    for each particular function, c.f., GNU attributes, fortran C
    annotation, etc.
*)


type 'a language

(** [register language arch name f] registers an abi with the given [name]
    and transformation [f] for the [language] and architecture [arch].
    Previous abi with the same name of this [language] and [arch], if
    any, is overwritten by the new value [f].*)
val register : 'a language -> arch -> string -> ('a -> sub term -> sub term) -> unit

(** [override_default language arch name] sets ABI [name] as a
    default ABI for the given [language] and architecture [arch] *)
val override_default : 'a language -> arch -> string -> unit

(** [override_resolver language arch resolve] overrides resolver for
    the the given [language] and architecture [arch]. For each
    function having a [name] and a list of [attrubutes] a
    [resolve api sub] evaluates to [Some abi] if for a function
    [sub] given [api] prescribes to use a non-default [abi]. If
    [None] is returned, then the default abi is used. *)
val override_resolver : 'a language -> arch -> ('a -> sub term -> string option) -> unit

(** [apply language arch api sub] applies a high-level [api],
    written in given [language], to a low-level abstraction of
    subroutine [sub] assuming given architecture [arch]. If
    knowledge is not applicable, then a subroutine is returned
    untouched.*)
val apply : 'a language -> arch -> 'a -> sub term -> sub term

(** [known langauge] returns a list of known targets and
    application binary interfaces. *)
val known : 'a language -> (arch * string list) list
