(** A repository of s-expressions.

    The repository holds an information about s-expressions loaded
    from files. The s-expression representation is indexed, and with
    each sub-tree of the s-expression we associate two indexes: the
    identity index and the equality index.

    The identity index uniquely identifies a tree, and location
    information is associated with each identity. The equality index
    represents structural equality and is in fact the ordinal number
    of the tree equivalence class in the quotient set by the
    structural equality. In other words, if two trees have the same
    equality index, then they are structurally equal.

    The source repository implements hashconsing, i.e., all trees with
    the same structure share the same memory regions.

    Note: despite the name the module doesn't have any dependencies on
    Primus Lisp, and works purely on the s-expression level. Later, we
    will move it into a separate library, along with the index and
    location modules. *)


module Index = Bap_primus_lisp_index
module Loc = Bap_primus_lisp_loc
module Id : Index.S
module Eq : Index.S

type error
type t
type 'a indexed = ('a,Id.t,Eq.t) Index.interned
type tree = token indexed
and token = Atom of string | List of tree list



(** [empty] source repository  *)
val empty : t

val is_empty : t -> bool

(** [load source filename] loads the source code from the given
    [filename]. The source code should be a sequence of well-formed
    s-expressions.

    The [filename] should be an explicit path.
*)
val load : t -> string -> (t,error) result


(** [find source filename] returns a list of trees loaded from a file
    with the given [filename]. *)
val find : t -> string -> tree list option


(** [loc source id] returns a location information for the identity
    with the provided [id].

    If there is no such information, then a bogus location is
    returned. *)
val loc : t -> Id.t -> Loc.t

(** [has_loc source id] if the location information is associated
    with the given [id] *)
val has_loc : t -> Id.t -> bool

(** [filename source id] returns the name of a file from which an
    identity with the given [id] is originating.

    If the identity is not known to the source code repository, then
    a bogus filename is returned. *)
val filename : t -> Id.t -> string

(** [fold source ~init ~f] iterates over all files loaded into the
    [source] repository.  *)
val fold : t -> init:'a -> f:(string -> tree list -> 'a -> 'a) -> 'a

val derived : t -> from:Id.t -> Id.t -> t

val lastid : t -> Id.t

val lasteq : t -> Eq.t


val pp_error : Format.formatter -> error -> unit

val pp_tree : Format.formatter -> tree -> unit


(** [pp_underline ?context src] creates an underlined source code printer.

    [let pp_loc = pp_underline ~context:n src] creates a function that
    takes a location and prints the corresponding original text of the
    location with the location being underlined with ['^'] and
    prefixed with ['>']. If context [n] is greater than [0], then [n]
    closest lines to the position are printed also and prefixed with
    ['|'].

    If the printed location spans over more than one line, then only
    the last line is underlined, however all lines are prefixed.*)
val pp_underline : ?context:int -> t -> Format.formatter -> Loc.t -> unit

(** [pp ?context src] creates a source code printer.

    [let pp_loc = pp src] creates a function that takes a location and
    prints the corresponding original text that is designated by the
    location. *)
val pp : t -> Format.formatter -> Loc.t -> unit
