open Core_kernel.Std
open Bap.Std
open Bap_c_type


class mapper = object(self)
  method map : t -> t = function
    | `Void -> self#map_void
    | `Array spec ->
      let spec = self#map_spec
          self#map_cvr_qualifier
          self#map_array spec in
      `Array spec
    | `Basic spec ->
      let spec = self#map_spec
          self#map_cv_qualifier
          self#map_basic spec in
      `Basic spec
    | `Pointer spec ->
      let spec = self#map_spec
          self#map_cvr_qualifier
          self#map spec in
      `Pointer spec
    | `Function spec ->
      let spec = self#map_spec
          self#map_no_qualifier
          self#map_proto spec in
      `Function spec
    | `Union spec ->
      let spec = self#map_spec
          self#map_no_qualifier
          self#map_compound spec in
      `Union spec
    | `Structure spec ->
      let spec = self#map_spec
          self#map_no_qualifier
          self#map_compound spec in
      `Structure spec

  method map_void : t = `Void


  method map_spec : 'a 'b. ('a -> 'a) -> ('b -> 'b) -> ('a,'b) spec -> ('a,'b) spec =
    fun map_qualifier map_t s -> Spec.{
        t = map_t s.t;
        qualifier = map_qualifier s.qualifier;
        attrs = self#map_attrs s.attrs
      }

  method map_basic = function
    | #integer as t -> self#map_integer t
    | #floating as t -> self#map_floating t

  method map_integer = function
    | #char as t   -> self#map_char t
    | #signed as t -> self#map_signed t
    | #unsigned as t -> self#map_unsigned t
    | `enum fields -> `enum (self#map_enum fields)

  method map_floating = function
    | #real as t -> self#map_real t
    | #complex as t -> self#map_complex t

  method map_char = ident
  method map_signed = ident
  method map_unsigned = ident
  method map_enum = ident
  method map_real = ident
  method map_complex = ident

  method map_cv_qualifier = ident
  method map_cvr_qualifier = ident
  method map_no_qualifier = ident

  method map_array s = Array.{
      element = self#map s.element;
      size = s.size;
    }

  method map_compound s = Compound.{
      name = self#map_name s.name;
      fields = self#map_fields s.fields;
    }

  method map_proto p = Proto.{
      return = self#map p.return;
      variadic = p.variadic;
      args = self#map_fields p.args
    }

  method map_fields =
    List.map ~f:(fun (n,t) -> self#map_name n, self#map t)

  method map_name = ident
  method map_attrs = List.map ~f:self#map_attr
  method map_attr = ident
end


module Make(M : Monad.S2) = struct
  open M.Monad_infix

  type 'e m = (t,'e) M.t
  type 'e u = (unit,'e) M.t

  class ['e] mapper = object(self)
    method private visit :
      'a. ('a -> 'e u) -> ('a -> ('a,'e) M.t) -> ('a -> 'e u) -> 'a -> ('a,'e) M.t=
      fun enter visit leave t ->
        enter t >>= fun () ->
        visit t >>= fun t  ->
        leave t >>= fun () ->
        M.return t

    method run = self#map_type


    method enter_type _ = M.return ()
    method leave_type _ = M.return ()
    method map_type =
      self#visit self#enter_type self#do_map_type self#leave_type

    method enter_void _ = M.return ()
    method leave_void _ = M.return ()
    method map_void =
      self#visit self#enter_void self#do_map_void self#leave_void

    method enter_array _ = M.return ()
    method leave_array _ = M.return ()
    method map_array =
      self#visit self#enter_array self#do_map_array self#leave_array

    method enter_basic _ = M.return ()
    method leave_basic _ = M.return ()
    method map_basic =
      self#visit self#enter_basic self#do_map_basic self#leave_basic

    method enter_pointer _ = M.return ()
    method leave_pointer _ = M.return ()
    method map_pointer =
      self#visit
        self#enter_pointer
        self#do_map_pointer
        self#leave_pointer

    method enter_function _ = M.return ()
    method leave_function _ = M.return ()
    method map_function =
      self#visit
        self#enter_function
        self#do_map_function
        self#leave_function

    method enter_union _ = M.return ()
    method leave_union _ = M.return ()
    method map_union =
      self#visit
        self#enter_union
        self#do_map_union
        self#leave_union

    method enter_structure _ = M.return ()
    method leave_structure _ = M.return ()
    method map_structure =
      self#visit
        self#enter_structure
        self#do_map_structure
        self#leave_structure

    method enter_cv_qualifier _ = M.return ()
    method leave_cv_qualifier _ = M.return ()
    method map_cv_qualifier =
      self#visit
        self#enter_cv_qualifier
        self#do_map_cv_qualifier
        self#leave_cv_qualifier

    method enter_cvr_qualifier _ = M.return ()
    method leave_cvr_qualifier _ = M.return ()
    method map_cvr_qualifier =
      self#visit
        self#enter_cvr_qualifier
        self#do_map_cvr_qualifier
        self#leave_cvr_qualifier

    method enter_integer _ = M.return ()
    method leave_integer _ = M.return ()
    method map_integer =
      self#visit
        self#enter_integer
        self#do_map_integer
        self#leave_integer

    method enter_floating _ = M.return ()
    method leave_floating _ = M.return ()
    method map_floating =
      self#visit
        self#enter_floating
        self#do_map_floating
        self#leave_floating

    method enter_char _ = M.return ()
    method leave_char _ = M.return ()
    method map_char =
      self#visit
        self#enter_char
        self#do_map_char
        self#leave_char

    method enter_signed _ = M.return ()
    method leave_signed _ = M.return ()
    method map_signed =
      self#visit
        self#enter_signed
        self#do_map_signed
        self#leave_signed

    method enter_unsigned _ = M.return ()
    method leave_unsigned _ = M.return ()
    method map_unsigned =
      self#visit
        self#enter_unsigned
        self#do_map_unsigned
        self#leave_unsigned

    method enter_enum _ = M.return ()
    method leave_enum _ = M.return ()
    method map_enum =
      self#visit
        self#enter_enum
        self#do_map_enum
        self#leave_enum

    method enter_real _ = M.return ()
    method leave_real _ = M.return ()
    method map_real =
      self#visit
        self#enter_real
        self#do_map_real
        self#leave_real

    method enter_complex _ = M.return ()
    method leave_complex _ = M.return ()
    method map_complex =
      self#visit
        self#enter_complex
        self#do_map_complex
        self#leave_complex


    method enter_name _ = M.return ()
    method leave_name _ = M.return ()
    method map_name =
      self#visit
        self#enter_name
        self#do_map_name
        self#leave_name

    method enter_attrs _ = M.return ()
    method leave_attrs _ = M.return ()
    method map_attrs =
      self#visit
        self#enter_attrs
        self#do_map_attrs
        self#leave_attrs

    method enter_attr _ = M.return ()
    method leave_attr _ = M.return ()
    method map_attr =
      self#visit
        self#enter_attr
        self#do_map_attr
        self#leave_attr

    method private do_map_type : t -> 'e m = function
      | `Void ->
        self#map_void `Void
      | `Array spec ->
        self#map_spec
          self#map_cvr_qualifier
          self#map_array spec >>| fun spec ->
        `Array spec
      | `Basic spec ->
        self#map_spec
          self#map_cv_qualifier
          self#map_basic spec >>| fun spec ->
        `Basic spec
      | `Pointer spec ->
        self#map_spec
          self#map_cvr_qualifier
          self#map_pointer spec >>| fun spec ->
        `Pointer spec
      | `Function spec ->
        self#map_spec
          self#map_no_qualifier
          self#map_function spec >>| fun spec ->
        `Function spec
      | `Union spec ->
        self#map_spec
          self#map_no_qualifier
          self#map_union spec >>| fun spec ->
        `Union spec
      | `Structure spec ->
        self#map_spec
          self#map_no_qualifier
          self#map_structure spec >>| fun spec ->
        `Structure spec

    method private do_map_void = M.return

    method private map_spec
      : 'a 'b. ('a -> ('a,'e) M.t) -> ('b -> ('b,'e) M.t) ->
        ('a,'b) spec -> (('a,'b) spec, 'e) M.t =
      fun map_qualifier map_t s ->
        let open Spec in
        map_t s.t >>= fun t ->
        map_qualifier s.qualifier >>= fun qualifier ->
        self#map_attrs s.attrs >>= fun attrs ->
        M.return {t; qualifier; attrs}

    method private do_map_basic = function
      | #integer as t ->
        self#map_integer t >>| fun x -> (x :> basic)
      | #floating as t ->
        self#map_floating t >>| fun x -> (x :> basic)

    method private do_map_pointer = self#map_type
    method private do_map_union = self#map_compound
    method private do_map_structure = self#map_structure

    method private do_map_integer : integer -> (integer,'e) M.t = function
      | #char as t     -> self#map_char t >>| fun x -> (x :> integer)
      | #signed as t   -> self#map_signed t >>| fun x -> (x :> integer)
      | #unsigned as t -> self#map_unsigned t >>| fun x -> (x :> integer)
      | `enum fields ->
        self#map_enum fields >>| fun fields ->
        `enum fields

    method private do_map_floating : floating -> (floating,'e) M.t = function
      | #real as t -> self#map_real t >>| fun t -> (t :> floating)
      | #complex as t -> self#map_complex t >>| fun t -> (t :> floating)

    method private do_map_char : char -> (char,'e) M.t = M.return
    method private do_map_signed : signed -> (signed,'e) M.t = M.return
    method private do_map_unsigned : unsigned -> (unsigned,'e) M.t = M.return
    method private do_map_enum = M.return
    method private do_map_real : real -> (real,'e) M.t = M.return
    method private do_map_complex : complex -> (complex,'e) M.t = M.return

    method private do_map_cv_qualifier = M.return
    method private do_map_cvr_qualifier = M.return
    method private map_no_qualifier = M.return

    method private do_map_array {Array.element;size} =
      self#map_type element >>| fun element ->
      {Array.element; size}

    method private map_compound {Compound.name; fields} =
      self#map_fields fields >>= fun fields ->
      self#map_name name >>= fun name ->
      M.return Compound.{name;fields}

    method private do_map_function {Proto.args; return; variadic} =
      self#map_type return >>= fun return ->
      self#map_fields args >>= fun args ->
      M.return { Proto.args; return; variadic}

    method map_fields fields =
      M.all @@ List.map fields ~f:(fun (n,t) ->
          self#map_name n >>= fun n ->
          self#map_type t >>= fun t ->
          M.return (n,t))

    method private do_map_name = M.return
    method private do_map_attrs attrs =
      M.all @@ List.map ~f:self#map_attr attrs
    method private do_map_attr = M.return
  end
end

module Ident2 : Monad.S2 with type ('a,'e) t = 'a = struct
  type ('a,'e) t = 'a
  include Monad.Make2(struct
      type nonrec ('a,'e) t = ('a,'e) t
      let return = ident
      let bind x f = f x
      let map = `Define_using_bind
    end)
end

module Finder = struct
  module SM = Monad.State
  type ('a,'e) t = ('a option, 'e option) SM.t
  open SM.Monad_infix

  let found x : ('a,'e) t =
    SM.put (Some x) >>= fun () ->
    SM.return None

  let result x : 'e option = SM.exec x None

  include Monad.Make2(struct
      type nonrec ('a,'e) t = ('a,'e) t
      let return x = SM.return (Some x)
      let bind x f =
        x >>= function
        | None -> SM.return None
        | Some x -> f x
      let map = `Define_using_bind
    end)

end


module Id = Make(Ident2)

module State = Make(Monad.State)

module OS = Monad.T.State.Make(Option)

module Find = Make(OS)
