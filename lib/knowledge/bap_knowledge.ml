open Core_kernel
open Monads.Std

module Order = struct
  type partial = LT | EQ | GT | NC
end

type conflict = ..

module Conflict = struct
  type t = conflict = ..
end

module Id = Int63

let user_package = "user"
let keyword_package = "keyword"


module Domain = struct
  type 'a t = {
    inspect : 'a -> Sexp.t;
    empty : 'a;
    order : 'a -> 'a -> Order.partial;
    name : string;
  }

  let define ?(inspect=sexp_of_opaque) ~empty ~order name = {
    inspect; empty; order; name;
  }

  let partial_of_total order x y : Order.partial = match order x y with
    | 0 -> EQ
    | 1 -> GT
    | _ -> LT

  let total ?inspect ~empty ~order name =
    define ?inspect ~empty name ~order:(partial_of_total order)

  let mapping (type k) (type o) ?(equal=(fun _ _ -> true))
      (module K : Comparator.S with type t = k
                                and type comparator_witness = o)
      name =
    let empty = Map.empty (module K) in
    let inspect xs =
      Sexp.List (Map.keys xs |> List.map ~f:K.comparator.sexp_of_t) in
    let order x y =
      Map.symmetric_diff x y ~data_equal:equal |>
      Sequence.fold ~init:(0,0,0) ~f:(fun (l,m,r) -> function
          | (_,`Left _)     -> (l+1,m,r)
          | (_,`Right _)    -> (l,m,r+1)
          | (_, `Unequal _) -> (l,m+1,r)) |> function
      | 0,0,0 -> Order.EQ
      | 0,0,_ -> LT
      | _,0,0 -> GT
      | _,_,_ -> NC in
    define ~inspect ~empty ~order name

  let optional ?inspect ~order name =
    let inspect = match inspect with
      | None -> sexp_of_opaque
      | Some sexp_of_elt -> sexp_of_option sexp_of_elt in
    let order x y : Order.partial = match x, y with
      | None,None -> EQ
      | None,Some _ -> LT
      | Some _,None -> GT
      | Some x, Some y -> partial_of_total order x y in
    let empty = None in
    define ~inspect ~order ~empty name

  let string = define "string" ~empty:""
      ~inspect:sexp_of_string ~order:(fun x y ->
          match String.is_empty x, String.is_empty y with
          | true, true -> EQ
          | true,false -> GT
          | false,true -> LT
          | false,false -> partial_of_total String.compare x y)
end

type 'a obj = Id.t

type fullname = {
  package : string;
  name : string;
}

module Registry = struct
  let packages = Hash_set.create (module String) ()
  let classes = Hashtbl.create (module String)
  let slots = Hashtbl.create (module String)

  let add_package name = Hash_set.add packages name

  let is_present ~package namespace name =
    match Hashtbl.find namespace package with
    | None -> false
    | Some names -> Map.mem names name

  let register kind namespace ?desc ?(package=user_package) name =
    add_package package;
    if is_present ~package namespace name
    then failwithf
        "Failed to declare new %s, there is already a %s \
         named `%s' in package `%s'" kind kind name package ();
    Hashtbl.update namespace package ~f:(function
        | None -> Map.singleton (module String) name desc
        | Some names -> Map.add_exn names ~key:name ~data:desc);
    {package; name}

  let add_class = register "class" classes
  let add_slot  = register "property" slots
end

let string_of_fname {package; name} =
  if package = keyword_package || package = user_package
  then name
  else package ^ ":" ^ name

let escaped =
  Staged.unstage @@
  String.Escaping.escape ~escapeworthy:[':'] ~escape_char:'\\'

let find_separator s =
  if String.is_empty s then None
  else String.Escaping.index s ~escape_char:'\\' ':'

(* invariant, keywords are always prefixed with [:] *)
let normalize_name ~package name =
  let package = escaped package in
  if package = keyword_package &&
     not (String.is_prefix ~prefix:":" name)
  then ":"^name else name

let split_name package s = match find_separator s with
  | None -> {package; name=s}
  | Some 0 -> {package=keyword_package; name=s}
  | Some len -> {
      package = String.sub s ~pos:0 ~len;
      name = String.subo s ~pos:(len+1);
    }

module Class = struct
  type top = unit

  type 'a data =
    | Top : top data
    | Derive : 'a data * 'b -> ('b -> 'a) data


  type 'a t = {
    id : Id.t;
    name : fullname;
    data : 'a data;
  }

  let classes = ref Id.zero

  let newclass ?desc ?package name data =
    Id.incr classes;
    {
      id = !classes;
      name = Registry.add_class ?desc ?package name;
      data;
    }

  let declare
    : ?desc:string -> ?package:string -> string -> 'a -> ('a -> top) t =
    fun ?desc ?package name data ->
      newclass ?desc ?package name (Derive (Top,data))

  let derived
    : ?desc:string -> ?package:string -> string -> 'a t -> 'b -> ('b -> 'a) t =
    fun ?desc ?package name parent data ->
      newclass ?desc ?package name (Derive (parent.data,data))

  let abstract : type a b. (b -> a) t -> a t = fun child -> match child with
    | {data=Derive (base,_); id; name} -> {id; name; data=base}

  let refine : type a b. a t -> b -> (b -> a) t =
    fun {id; name; data} data' -> {id; name; data=Derive (data,data')}

  let same x y = Id.equal x.id y.id

  let equal : type a b. a t -> b t -> (a obj,b obj) Type_equal.t option =
    fun x y -> Option.some_if (same x y) Type_equal.T

  let assert_equal x y = match equal x y with
    | Some t -> t
    | None ->
      failwithf "assert_equal: wrong assertion, classes of %s and %s \
                 are different"
        (string_of_fname x.name)
        (string_of_fname y.name)
        ()

  let data : type a b. (b -> a) t -> b = fun {data=Derive (_,data)} -> data

  let name {name={name}} = name
  let package {name={package}} = package
  let fullname {name} = string_of_fname name

end

module Record = struct
  module Dict = Univ_map
  type  t = Dict.t
  module Repr = struct
    type entry = {
      name : string;
      data : string;
    } [@@deriving bin_io]

    type t = entry list [@@deriving bin_io]
  end

  type univ = Dict.Packed.t


  type slot_io = {
    reader : string -> univ;
    writer : univ -> string;
  }

  type slot_domain = {
    inspect : univ -> Sexp.t;
    order  : univ -> univ -> Order.partial;
    empty  : univ;
  }

  let io : slot_io Hashtbl.M(String).t =
    Hashtbl.create (module String)

  let domains : slot_domain Hashtbl.M(String).t =
    Hashtbl.create (module String)

  let empty = Dict.empty

  let (<:=) x y =
    Dict.to_alist x |> List.for_all ~f:(fun (Dict.Packed.T (k,_) as x) ->
        match Dict.find y k with
        | None -> false
        | Some y ->
          let dom = Hashtbl.find_exn domains (Dict.Key.name k) in
          match dom.order x (Dict.Packed.T (k,y)) with
          | LT | EQ -> true
          | GT | NC -> false)

  let order : t -> t -> Order.partial = fun x y ->
    match x <:= y, y <:= x with
    | true,false  -> LT
    | true,true   -> EQ
    | false,true  -> GT
    | false,false -> NC


  let put key v x = Dict.set v key x
  let get key {Domain.empty} data = match Dict.find data key with
    | None -> empty
    | Some x -> x

  exception Merge_conflict of string * Sexp.t * Sexp.t

  type conflict += Merge of string * Sexp.t * Sexp.t

  let merge ~on_conflict x y =
    Dict.to_alist x |>
    List.fold ~init:y ~f:(fun v (Dict.Packed.T (k,x)) ->
        Dict.change v k ~f:(function
            | None -> Some x
            | Some y ->
              let name = Dict.Key.name k in
              let pack v = Dict.Packed.T (k,v) in
              let dom = Hashtbl.find_exn domains name in
              match dom.order (pack x) (pack y) with
              | EQ | LT -> Some x
              | GT -> Some y
              | NC -> match on_conflict with
                | `drop_both -> None
                | `drop_left -> Some y
                | `drop_right -> Some x
                | `fail ->
                  let x = dom.inspect (pack x)
                  and y = dom.inspect (pack y) in
                  raise (Merge_conflict (name,x,y))))

  let join x y =
    try Ok (merge ~on_conflict:`fail x y)
    with Merge_conflict (n,x,y) -> Error (Merge (n,x,y))

  let register_persistent (type p)
      (key : p Dict.Key.t)
      (module P : Binable.S with type t = p) =
    let slot = Dict.Key.name key in
    let writer (Dict.Packed.T (k,x)) =
      match Type_equal.Id.same_witness k key with
      | Some Type_equal.T -> Binable.to_string (module P) x
      | None -> assert false in
    let reader s =
      Dict.Packed.T (key,Binable.of_string (module P) s) in
    Hashtbl.add_exn io ~key:slot ~data:{reader;writer}

  include Binable.Of_binable(Repr)(struct
      type t = Dict.t
      let to_binable s =
        Dict.to_alist s |>
        List.rev_filter_map ~f:(fun (Dict.Packed.T(k,_) as x) ->
            let name = Dict.Key.name k in
            match Hashtbl.find io name with
            | None -> None
            | Some {writer=to_string} ->
              Some Repr.{name; data = to_string x;})

      let of_binable entries =
        List.fold entries ~init:Dict.empty ~f:(fun s {Repr.name; data} ->
            match Hashtbl.find io name with
            | None -> s
            | Some {reader=parse} ->
              let Dict.Packed.T (key,data) = parse data in
              Dict.set s key data)
    end)

  let register_domain
    : type p. p Dict.Key.t -> p Domain.t -> unit =
    fun key dom ->
      let name = Dict.Key.name key in
      let eq = Type_equal.Id.same_witness_exn in
      let order (Dict.Packed.T (kx,x)) (Dict.Packed.T (ky,y)) =
        let Type_equal.T = eq kx ky in
        let Type_equal.T = eq kx key in
        dom.order x y in
      let empty = Dict.Packed.T(key, dom.empty) in
      let inspect (Dict.Packed.T(kx,x)) =
        let Type_equal.T = eq kx key in
        dom.inspect x in
      Hashtbl.add_exn domains ~key:name ~data:{
        inspect;
        empty;
        order;
      }

  let sexp_of_t x =
    Sexp.List (Dict.to_alist x |> List.map ~f:(function
          Dict.Packed.T (k,x) ->
          let name = Dict.Key.name k in
          let dom = Hashtbl.find_exn domains name in
          Sexp.List [
            Atom name;
            dom.inspect (Dict.Packed.T(k,x));
          ]))

  let t_of_sexp = opaque_of_sexp

  let inspect = sexp_of_t
end

module Knowledge = struct

  type 'a value = {
    cls  : 'a Class.t;
    data : Record.t;
    time : Id.t;
  }
  type 'a cls = 'a Class.t
  type 'a obj = Id.t
  type 'p domain = 'p Domain.t
  type 'a ord = Id.comparator_witness
  type conflict = Conflict.t = ..

  module Pid = Int63
  type pid = Pid.t


  module Data = struct
    type objects = {
      vals : Record.t Map.M(Id).t;
      reqs : Set.M(Id).t Map.M(Pid).t;
      objs : Id.t Map.M(String).t Map.M(String).t;
      syms : fullname Map.M(Id).t;
      pubs : Set.M(Id).t Map.M(String).t;
    }

    let empty_class = {
      vals = Map.empty (module Id);
      reqs = Map.empty (module Pid);
      objs = Map.empty (module String);
      syms = Map.empty (module Id);
      pubs = Map.empty (module String);
    }

    type t = {
      classes : objects Map.M(Id).t;
      package : string;
    }
  end

  type state = Data.t
  let empty : Data.t = {
    package = user_package;
    classes = Map.empty (module Id);
  }

  module State = struct
    include Monad.State.T1(Data)(Monad.Ident)
    include Monad.State.Make(Data)(Monad.Ident)
  end

  module Knowledge = struct
    type 'a t = ('a,conflict) Result.t State.t
    include Monad.Result.Make(Conflict)(State)
  end

  type 'a knowledge = 'a Knowledge.t

  open Knowledge.Syntax

  module Slot = struct
    type 'p promise = {
      get : Id.t -> 'p Knowledge.t;
      pid : pid;
    }

    type ('a,'p) t = {
      cls : 'a Class.t;
      dom : 'p Domain.t;
      key : 'p Type_equal.Id.t;
      name : string;
      desc : string option;
      mutable promises : 'p promise list;
    }

    let declare ?desc ?persistent ?package cls name (dom : 'a Domain.t) =
      let slot = Registry.add_slot ?desc ?package name in
      let name = string_of_fname slot in
      let key = Type_equal.Id.create ~name dom.inspect in
      Option.iter persistent (Record.register_persistent key);
      Record.register_domain key dom;
      {cls; dom; key; name; desc; promises = []}
  end

  type ('a,'p) slot = ('a,'p) Slot.t

  module Value = struct
    type 'a t = 'a value

    (* we could use an extension variant or create a new OCaml object
       instead of incrementing a second, but they are less reliable
       and heavier *)
    let next_second =
      let current = ref Int63.zero in
      fun () -> Int63.incr current; !current

    let empty cls =
      {cls; data=Record.empty; time = next_second ()}

    let clone cls {data; time} = {cls; data; time}
    let cls {cls} = cls
    let create cls data = {cls; data; time = next_second ()}
    let put {Slot.key} v x = {
      v with data = Record.put key v.data x;
             time = next_second ()
    }
    let get {Slot.key; dom} {data} = Record.get key dom data
    let strip
      : type a b. (a value, b value) Type_equal.t -> (a,b) Type_equal.t =
      fun T -> T

    type strategy = [`drop_left | `drop_right | `drop_both]

    let merge ?(on_conflict=`drop_old) x y =
      let on_conflict : strategy = match on_conflict with
        | `drop_old -> if Id.(x.time < y.time)
          then `drop_left else `drop_right
        | `drop_new -> if Id.(x.time < y.time)
          then `drop_right else `drop_left
        | #strategy as other -> other in
      {
        x with time = next_second ();
               data = Record.merge ~on_conflict x.data y.data
      }


    let join x y = match Record.join x.data y.data with
      | Ok data -> Ok {x with data; time = next_second ()}
      | Error c -> Error c

    module type S = sig
      type t [@@deriving sexp]
      include Base.Comparable.S with type t := t
      include Binable.S with type t := t
    end

    module Comparator = Base.Comparator.Make1(struct
        type 'a t = 'a value
        let sexp_of_t = sexp_of_opaque
        let compare x y = match Record.order x.data y.data with
          | LT -> -1
          | EQ -> 0
          | GT -> 1
          | NC -> Int63.compare x.time y.time
      end)

    include Comparator

    type 'a ord = comparator_witness

    let derive
      : type a. a cls ->
        (module S with type t = a t and type comparator_witness = a ord) =
      fun cls ->
        let module R = struct
          type t = a value
          let sexp_of_t x = Record.sexp_of_t x.data
          let t_of_sexp = opaque_of_sexp
          include Binable.Of_binable(Record)(struct
              type t = a value
              let to_binable : 'a value -> Record.t =
                fun {data} -> data
              let of_binable : Record.t -> 'a value =
                fun data -> {cls; data; time = next_second ()}
            end)
          type comparator_witness = Comparator.comparator_witness
          include Base.Comparable.Make_using_comparator(struct
              type t = a value
              let sexp_of_t = sexp_of_t
              include Comparator
            end)
        end in
        (module R)


  end

  module Class = struct
    include Class
    let property = Slot.declare
    let strip
      : type a b. (a t, b t) Type_equal.t -> (a,b) Type_equal.t =
      fun T -> T
  end

  let get () = Knowledge.lift (State.get ())
  let put s = Knowledge.lift (State.put s)
  let gets f = Knowledge.lift (State.gets f)
  let update f = Knowledge.lift (State.update f)

  let provide : type a p. (a,p) slot -> a obj -> p -> unit Knowledge.t =
    fun slot obj x -> update @@ function {classes} as s ->
        let {Data.vals} as objs =
          match Map.find classes slot.cls.id with
          | None -> Data.empty_class
          | Some objs -> objs in {
          s with classes = Map.set classes ~key:slot.cls.id ~data:{
            objs with vals = Map.update vals obj ~f:(function
            | None -> Record.(put slot.key empty x)
            | Some v -> Record.put slot.key v x)
          }
        }

  let pids = ref Pid.zero

  let promise (s : _ slot) get =
    Pid.incr pids;
    let pid = !pids in
    s.promises <- {get;pid} :: s.promises

  let is_active reqs pid id = match Map.find reqs pid with
    | None -> false
    | Some ids -> Set.mem ids id

  let activate reqs pid id = Map.update reqs pid ~f:(function
      | None -> Set.singleton (module Pid) id
      | Some ids -> Set.add ids id)

  let deactivate req pid id = Map.change req pid ~f:(function
      | None -> None
      | Some ids ->
        let ids = Set.remove ids id in
        if Set.is_empty ids then None else Some ids)

  let objects : _ cls -> _ = fun cls ->
    get () >>| fun {classes} ->
    match Map.find classes cls.id with
    | None -> Data.empty_class
    | Some objs -> objs

  let update_reqs : _ slot -> _ = fun slot reqs ->
    update @@ function {classes} as s -> {
        s with classes = Map.update classes slot.cls.id ~f:(function
        | None -> {Data.empty_class with reqs}
        | Some objs -> {objs with reqs})
      }

  let collect : type a p. (a,p) slot -> a obj -> p Knowledge.t =
    fun slot id ->
      objects slot.cls >>= fun {Data.vals} ->
      let init = match Map.find vals id with
        | None -> slot.dom.empty
        | Some v -> Record.get slot.key slot.dom v in
      slot.promises |>
      Knowledge.List.fold ~init ~f:(fun curr req ->
          objects slot.cls >>= fun {Data.reqs} ->
          if is_active reqs req.pid id
          then Knowledge.return curr
          else
            update_reqs slot (activate reqs req.pid id) >>= fun () ->
            req.get id >>= fun next ->
            update_reqs slot (deactivate reqs req.pid id) >>= fun () ->
            match slot.dom.order curr next with
            | GT -> Knowledge.return curr
            | LT | EQ | NC -> Knowledge.return next) >>= fun max ->
      provide slot id max >>| fun () -> max



  module Object = struct
    type +'a t = 'a obj
    type 'a ord = Id.comparator_witness

    let with_new_object objs f = match Map.max_elt objs.Data.vals with
      | None -> f Id.one {
          objs
          with vals = Map.singleton (module Id) Id.one Record.empty
        }
      | Some (key,_) ->
        let key = Id.succ key in
        f key {
          objs
          with vals = Map.add_exn objs.vals ~key ~data:Record.empty
        }

    let create : 'a cls -> 'a obj Knowledge.t = fun cls ->
      objects cls >>= fun objs ->
      with_new_object objs @@ fun obj objs ->
      update @@begin function {classes} as s -> {
          s with classes = Map.set classes ~key:cls.id ~data:objs
        }
      end >>| fun () ->
      obj

    let delete {Class.id} obj =
      update @@ function {classes} as s -> {
          s with
          classes = Map.change classes id ~f:(function
              | None -> None
              | Some objs ->
                let vals = Map.remove objs.vals obj in
                if Map.is_empty vals then None
                else Some {objs with vals})
        }


    let scoped cls scope =
      create cls >>= fun obj ->
      scope  obj >>= fun r ->
      delete cls obj >>| fun () ->
      r


    let do_intern =
      let is_public ~package name {Data.pubs} =
        match Map.find pubs package with
        | None -> false
        | Some pubs -> Set.mem pubs name in
      let unchanged id = Knowledge.return id in
      let publicize ~package obj: Data.objects -> Data.objects =
        fun objects -> {
            objects with pubs = Map.update objects.pubs package ~f:(function
            | None -> Set.singleton (module Id) obj
            | Some pubs -> Set.add pubs obj)
          } in
      let createsym ~public ~package name classes clsid objects s =
        with_new_object objects @@ fun obj objects ->
        let syms = Map.set objects.syms obj {package; name} in
        let objs = Map.update objects.objs package ~f:(function
            | None -> Map.singleton (module String) name obj
            | Some names -> Map.set names name obj) in
        let objects = {objects with objs; syms} in
        let objects = if public
          then publicize ~package obj objects else objects in
        put {s with classes = Map.set classes clsid objects} >>| fun () ->
        obj in

      fun ?(public=false) ?desc:_ ?package name {Class.id} ->
        get () >>= fun ({classes} as s) ->
        let package = Option.value package ~default:s.package in
        let name = normalize_name ~package name in
        let objects = match Map.find classes id with
          | None -> Data.empty_class
          | Some objs -> objs in
        match Map.find objects.objs package with
        | None -> createsym ~public ~package name classes id objects s
        | Some names -> match Map.find names name with
          | None -> createsym ~public ~package name classes id objects s
          | Some obj when not public -> unchanged obj
          | Some obj ->
            if is_public ~package obj objects then unchanged obj
            else
              let objects = publicize ~package obj objects in
              put {s with classes = Map.set classes id objects} >>| fun () ->
              obj

    (* any [:] in names here are never treated as separators,
       contrary to [read], where they are, and [do_intern] where
       a leading [:] in a name will be left for keywords *)
    let intern ?public ?desc ?package name cls =
      let name = escaped name in
      do_intern ?public ?desc ?package name cls

    let uninterned_repr cls obj =
      Format.asprintf "#<%s %a>" cls Id.pp obj

    let repr {Class.name=cls as fname; id=cid} obj =
      get () >>= fun {package; classes} ->
      let cls = if package = cls.package then cls.name
        else string_of_fname fname in
      Knowledge.return @@
      match Map.find classes cid with
      | None -> uninterned_repr cls obj
      | Some {syms} -> match Map.find syms obj with
        | Some fname -> if fname.package = package
          then fname.name
          else string_of_fname fname
        | None -> uninterned_repr cls obj


    let read cls input =
      if String.length input < 2 then
        Scanf.sscanf input "#<%s %s>" @@ fun _ obj ->
        Knowledge.return (Id.of_string obj)
      else
        get () >>= fun {Data.package} ->
        let {package; name} = split_name package input in
        do_intern ~package name cls

    let cast : type a b. (a obj, b obj) Type_equal.t -> a obj -> b obj =
      fun Type_equal.T x -> x

    let id x = x

    let comparator : type a. a cls ->
      (module Comparator.S
        with type t = a obj
         and type comparator_witness = a ord) = fun _ ->
      let module R = struct
        type t = a obj
        type comparator_witness = a ord
        let comparator = Id.comparator
      end in
      (module R)
  end

  module Domain = struct
    include Domain

    let inspect_obj name x =
      Sexp.Atom (Format.asprintf "#<%s %a>" name Id.pp x)

    let obj {Class.name} =
      let name = string_of_fname name in
      total ~inspect:(inspect_obj name) ~empty:Id.zero
        ~order:Id.compare name

  end
  module Order = Order

  module Symbol = struct
    let intern = Object.intern
    let keyword = keyword_package

    let in_package package f =
      get () >>= function {Data.package=old_package} as s ->
        put {s with package} >>= fun () ->
        f () >>= fun r ->
        update (fun s -> {s with package = old_package}) >>| fun () ->
        r


    type conflict += Import of fullname * fullname

    let intern_symbol ~package name obj cls =
      Knowledge.return Data.{
          cls
          with objs = Map.update cls.objs package ~f:(function
              | None -> Map.singleton (module String) name obj
              | Some names -> Map.set names name obj)}



    (* imports names inside a class.

       All names that [needs_import] will be imported
       into the [package]. If the [package] already had
       the same name but with different value, then a
       [strict] import will raise an error, otherwise it
       will be overwritten with the new value.
    *)
    let import_class ~strict ~package ~needs_import
      : Data.objects -> Data.objects knowledge
      = fun cls ->
        Map.to_sequence cls.syms |>
        Knowledge.Seq.fold ~init:cls ~f:(fun cls (obj,sym) ->
            if not (needs_import cls sym obj)
            then Knowledge.return cls
            else
              let obj' = match Map.find cls.objs package with
                | None -> Id.zero
                | Some names -> match Map.find names sym.name with
                  | None -> Id.zero
                  | Some obj' -> obj' in
              if not strict || Id.(obj' = zero || obj' = obj)
              then intern_symbol ~package sym.name obj cls
              else
                let sym' = Map.find_exn cls.syms obj' in
                Knowledge.fail (Import (sym,sym')))

    let package_exists package = Map.exists ~f:(fun {Data.objs} ->
        Map.mem objs package)

    let name_exists {package; name} = Map.exists ~f:(fun {Data.objs} ->
        match Map.find objs package with
        | None -> false
        | Some names -> Map.mem names name)


    type conflict += Not_a_package of string
    type conflict += Not_a_symbol of fullname

    let check_name classes = function
      | `Pkg name -> if package_exists name classes
        then Knowledge.return ()
        else Knowledge.fail (Not_a_package name)
      | `Sym sym -> if name_exists sym classes
        then Knowledge.return ()
        else Knowledge.fail (Not_a_symbol sym)

    let current = function
      | Some p -> Knowledge.return p
      | None -> gets (fun s -> s.package)

    let import ?(strict=false) ?package imports : unit knowledge =
      current package >>| escaped >>= fun package ->
      get () >>= fun s ->
      Knowledge.List.fold ~init:s.classes imports ~f:(fun classes name ->
          let name = match find_separator name with
            | None -> `Pkg name
            | Some _ -> `Sym (split_name package name) in
          let needs_import {Data.pubs} sym obj = match name with
            | `Sym s -> sym = s
            | `Pkg p -> match Map.find pubs p with
              | None -> false
              | Some pubs -> Set.mem pubs obj in
          check_name classes name >>= fun () ->
          Map.to_sequence classes |>
          Knowledge.Seq.fold ~init:classes
            ~f:(fun classes (clsid,objects) ->
                import_class ~strict ~package ~needs_import objects
                >>| fun objects ->
                Map.set classes clsid objects))
      >>= fun classes -> put {s with classes}
  end


  module Syntax = struct
    include Knowledge.Syntax
    let (-->) x p = collect p x
    let (>>>) p f = promise p f
    let (//) c s = Object.read c s
  end

  module type S = sig
    include Monad.S with type 'a t = 'a knowledge
                     and module Syntax := Syntax
    include Monad.Fail.S with type 'a t := 'a knowledge
                          and type 'a error = conflict
  end
  include (Knowledge : S)

  let get_value cls obj = objects cls >>| fun {Data.vals} ->
    match Map.find vals obj with
    | None -> Value.empty cls
    | Some x -> Value.create cls x

  let run cls obj s =
    match State.run (obj >>= get_value cls) s with
    | Ok x,s -> Ok (x,s)
    | Error err,_ -> Error err

end

type 'a knowledge = 'a Knowledge.t
