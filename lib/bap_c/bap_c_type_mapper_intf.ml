open Bap_c_type

(** Type mapper, visitor, iterator, finder all in one.

    The interface is wrapped into a monad, that allows, by choosing a
    proper monad implement all the above morphisms and iterators.

    Each syntactical element [t:T] of the type system is represented
    with three methods:

    - [enter_T t]
    - [map_T t]
    - [leave_T t]

    The [map_T t] method first calls [enter_T t], the applies a deep
    mapping of the [t] to [t'] and finally calls [leave_T t'].


    Override [enter_T] if an element shouldn't be morphed. The
    combination of [enter_t], [leave_T] allows to perform different
    visiting strategies. If mapping is needed then a [map_T] method
    should be overriden. A usual pattern would be:

    {[
      class my_mapper = object(self)
        inherit base as super

        method map_T t =
          super#map_T t >>| self#my_transformation

        method private my_transformation t = t
      end
    ]}
*)
module type S = sig
  type ('a,'e) m

  class ['e] base : object
    method run : t -> (t,'e) m

    method enter_type : t -> (unit,'e) m
    method map_type   : t -> (t,'e) m
    method leave_type : t -> (unit,'e) m

    method enter_void : t -> (unit,'e) m
    method map_void   : t -> (t,'e) m
    method leave_void : t -> (unit,'e) m


    method enter_array : array -> (unit,'e) m
    method map_array   : array -> (array,'e) m
    method leave_array : array -> (unit,'e) m

    method enter_pointer : t -> (unit,'e) m
    method map_pointer   : t -> (t,'e) m
    method leave_pointer : t -> (unit,'e) m

    method enter_function : proto -> (unit,'e) m
    method map_function   : proto -> (proto,'e) m
    method leave_function : proto -> (unit,'e) m

    method enter_union : compound -> (unit,'e) m
    method map_union   : compound -> (compound,'e) m
    method leave_union : compound -> (unit,'e) m

    method enter_structure : compound -> (unit,'e) m
    method map_structure   : compound -> (compound,'e) m
    method leave_structure : compound -> (unit,'e) m

    method enter_cv_qualifier : cv qualifier -> (unit,'e) m
    method map_cv_qualifier   : cv qualifier -> (cv qualifier,'e) m
    method leave_cv_qualifier : cv qualifier -> (unit,'e) m

    method enter_cvr_qualifier : cvr qualifier -> (unit,'e) m
    method map_cvr_qualifier   : cvr qualifier -> (cvr qualifier,'e) m
    method leave_cvr_qualifier : cvr qualifier -> (unit,'e) m

    method enter_basic : basic -> (unit,'e) m
    method map_basic   : basic -> (basic,'e) m
    method leave_basic : basic -> (unit,'e) m

    method enter_integer : integer -> (unit,'e) m
    method map_integer   : integer -> (integer,'e) m
    method leave_integer : integer -> (unit,'e) m

    method enter_char : char -> (unit,'e) m
    method map_char   : char -> (char,'e) m
    method leave_char : char -> (unit,'e) m

    method enter_signed : signed -> (unit,'e) m
    method map_signed   : signed -> (signed,'e) m
    method leave_signed : signed -> (unit,'e) m

    method enter_unsigned : unsigned -> (unit,'e) m
    method map_unsigned   : unsigned -> (unsigned,'e) m
    method leave_unsigned : unsigned -> (unit,'e) m

    method enter_enum : (string * int64 option) list -> (unit,'e) m
    method map_enum   : (string * int64 option) list -> ((string * int64 option) list,'e) m
    method leave_enum : (string * int64 option) list -> (unit,'e) m

    method enter_floating : floating -> (unit,'e) m
    method map_floating   : floating -> (floating,'e) m
    method leave_floating : floating -> (unit,'e) m

    method enter_real : real -> (unit,'e) m
    method map_real   : real -> (real,'e) m
    method leave_real : real -> (unit,'e) m

    method enter_complex : complex -> (unit,'e) m
    method map_complex   : complex -> (complex,'e) m
    method leave_complex : complex -> (unit,'e) m

    method enter_attrs : attr list -> (unit,'e) m
    method map_attrs   : attr list -> (attr list,'e) m
    method leave_attrs : attr list -> (unit,'e) m

    method enter_attr : attr -> (unit,'e) m
    method map_attr   : attr -> (attr,'e) m
    method leave_attr : attr -> (unit,'e) m

    method enter_name : string -> (unit,'e) m
    method map_name   : string -> (string,'e) m
    method leave_name : string -> (unit,'e) m
  end
end
