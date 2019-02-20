open Core_kernel


module Id = struct
  include Int63
  let root = Int63.zero
end

type 'a obj = Id.t
type 'a t = 'a obj

module Class = struct
  module type Name = sig
    val name : string
  end

  module type S = sig
    type cls
    include Name
    include Base.Comparable.S
      with type t = cls t
       and type comparator_witness = cls
  end


  module Make(N : Name)() : S = struct
    type cls = Id.comparator_witness
    include N
    include (Id : Base.Comparable.S
             with type t = cls obj
              and type comparator_witness = cls)
  end

  type 'a t = (module S with type cls = 'a)
end

type names = Id.t Map.M(String).t

let next : type a. a Class.t -> names ->
  (names -> a obj -> 'r) -> 'r =
  fun (module Class) ns k ->
    let ns = Map.update ns Class.name ~f:(function
        | None -> Id.one
        | Some x -> Id.succ x) in
    k ns (Map.find_exn ns Class.name)

module Var = Class.Make(struct
    let name = "var"
  end)()


module Fun = Class.Make(struct
    let name = "fun"
  end)()

type vars = Set.M(Var).t
type funs = Set.M(Fun).t

let vars : vars = Set.empty (module Var)
let funs : funs = Set.empty (module Fun)
let put set (x : 'a obj) = Set.add set x

module type Example = sig
  type +'a cls
  type +'a obj
  type +'a ord
  type (+'a, 'b) slot
  type 'a property

  module Class : sig
    val declare : ?desc:string -> string -> 'a cls

    val derive : ?desc:string -> string -> 'a cls -> ('b -> 'a) cls

    val comparator : 'a cls -> (module Comparator.S
                                 with type t = 'a obj
                                  and type comparator_witness = 'a ord)

    val parent : (_ -> 'a) cls -> 'a cls
  end

  module Object : sig
    val create : 'a cls -> 'a obj
    val base : ('a -> 'b) obj -> 'b obj
    val cls : 'a obj -> 'a cls
  end


  module Slot : sig
    val declare : ?desc:string -> string -> 'a cls -> 'b property -> ('a,'b) slot
    val get : ('a,'b) slot -> 'a obj -> 'b
    val set : ('a,'b) slot -> 'a obj -> 'b -> unit
  end
end

module E : Example = struct
  module Ptr = Int63
  type ptr = Ptr.t
  type +'a obj = ptr
  type +'a cls = int
  type +'a ord = Ptr.comparator_witness
  type 'a property = 'a Type_equal.Id.t
  type 'a value
  type (+'a,'b) slot = {
    cls : 'a cls;
    pos : int;
    key : 'b property;
  }


  let classes = ref 0
  let parents = Hashtbl.create (module Int)

  module Class = struct
    let declare ?desc:_ _ : 'a cls =
      incr classes;
      !classes

    let derive ?desc name parent =
      let child = declare ?desc name in
      Hashtbl.add_exn parents ~key:child ~data:parent;
      child

    let parent child : 'a cls = Hashtbl.find_exn parents child


    let comparator : type a. a cls ->
      (module Comparator.S
        with type t = a obj
         and type comparator_witness = a ord) = fun _ ->
      let module R = struct
        type t = a obj
        type comparator_witness = Ptr.comparator_witness
        let comparator = Ptr.comparator
      end in
      (module R)
  end

  module Object = struct
    (* it will be a monad, IRL *)
    let objects = ref Ptr.zero
    let classes = Hashtbl.create (module Int63)
    let create : 'a cls -> 'a obj = fun cls ->
      Ptr.incr objects;
      Hashtbl.add_exn classes ~key:!objects ~data:cls;
      !objects

    let base x = x
    let cls x = Hashtbl.find_exn classes x
  end

  module Slot = struct
    let slots = Hashtbl.create (module Int)
    let heap = Hashtbl.create (module Int)

    let declare ?desc:_ _ cls key =
      Hashtbl.incr slots cls;
      {cls; pos = Hashtbl.find_exn slots cls; key}

    let get {cls;pos;key} obj = assert false


  end
end

module Play = struct
  open E

  type sort
  type bitv
  type signed

  let sort : sort cls = Class.declare "sort"
  let bitv : (bitv -> sort) cls  = Class.derive "bitv" sort
  let signed : (signed -> bitv -> sort) cls = Class.derive "signed" bitv

  let bitv' = Class.parent signed
  let sort' = Class.parent bitv'

  let sc = Class.comparator signed

  let e1 = Set.empty (Class.comparator signed)
  let e2 = Set.empty (Class.comparator signed)
  let e3 = Set.empty (Class.comparator (Class.parent signed))

  let x = Object.create signed

  let y = Object.base x

  let _ = y = (Object.base x)

  let e = Set.add e1 x
  let e' = Set.add e3 (Object.base x)


end
