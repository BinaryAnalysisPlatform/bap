open Core_kernel
open Bap.Std
open Bap_primus.Std

include Self()

module Region = struct
  type point = Primus.Value.t [@@deriving compare, sexp_of]
  type t = {
    lower : point;
    upper : point;
  } [@@deriving compare, fields, sexp_of]
end

module Regions = Interval_tree.Make(Region)

type t = {
  regions : unit Regions.t Primus.Value.Map.t;
}


let state = Primus.Machine.State.declare
    ~uuid:"0FC4AF7F-7237-4CE1-874A-4971801C6D67"
    ~name:"region"
    (fun _ -> {
         regions = Primus.Value.Map.empty;
       })

module Pre(Machine : Primus.Machine.S) = struct
  include Machine.Syntax
  module Lisp = Primus.Lisp.Make(Machine)
  module Value = Primus.Value.Make(Machine)
  let nil = Value.zero 1
  let bool = function
    | true -> nil
    | false -> Value.one 1_
  let failf = Lisp.failf
end

module Create(Machine : Primus.Machine.S) = struct
  include Pre(Machine)
  [@@@warning "-P"]

  let run  [reg; lower; upper] =
    Machine.Local.update state ~f:(fun {regions} -> {
          regions = Map.update regions reg ~f:(function
              | None -> Regions.singleton {lower; upper} ()
              | Some regs -> Regions.add regs {lower;upper} ())
        }) >>| fun () -> reg
end

module Lower(Machine : Primus.Machine.S) = struct
  include Pre(Machine)
  [@@@warning "-P"]

  let run [reg; addr] =
    Machine.Local.get state >>= (fun {regions} ->
        match Map.find regions reg with
        | None -> nil
        | Some map -> match Seq.hd (Regions.lookup map addr) with
          | None -> nil
          | Some ({lower},()) -> Machine.return lower)
end

module Upper(Machine : Primus.Machine.S) = struct
  include Pre(Machine)
  [@@@warning "-P"]

  let run [reg; addr] =
    Machine.Local.get state >>= (fun {regions} ->
        match Map.find regions reg with
        | None -> nil
        | Some map ->
          match Seq.to_list_rev (Regions.lookup map addr) with
          | [] -> nil
          | ({upper},())::_ -> Machine.return upper)
end

module Count(Machine : Primus.Machine.S) = struct
  include Pre(Machine)
  [@@@warning "-P"]

  let run [reg] =
    Machine.gets Project.arch >>= fun arch ->
    let width = Size.in_bits @@ Arch.addr_size arch in
    Machine.Local.get state >>= (fun {regions} ->
        match Map.find regions reg with
        | None -> nil
        | Some map -> Value.of_int ~width (Regions.length map))
end

module Move(Machine : Primus.Machine.S) = struct
  include Pre(Machine)
  [@@@warning "-P"]
  let run [dst; src; addr] =
    Machine.Local.get state >>= fun {regions} ->
    match Map.find regions src with
    | None -> nil
    | Some reg ->
      let reg' = match Map.find regions dst with
        | None -> Regions.empty
        | Some reg' -> reg' in
      let reg,reg',n =
        Regions.lookup reg addr |>
        Seq.fold ~init:(reg,reg',0) ~f:(fun (reg,reg',n) (r,()) ->
            Regions.remove reg r,
            Regions.add reg' r (),
            n + 1) in
      let regions =
        Map.set (Map.set regions ~key:src ~data:reg) ~key:dst ~data:reg' in
      Machine.Local.put state {regions} >>= fun () ->
      Value.of_bool (n > 0)
end

module Main(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  open Primus.Lisp.Type.Spec
  let def name types closure docs =
    Lisp.define ~docs ~types name closure
  let init () =
    Machine.sequence [
      def "region-create" (tuple [sym; int; int] @-> sym)
        (module Create)
        {|(region-create ID LOWER UPPER) adds [LOWER,UPPER] to the set ID.

          Adds a region denoted with the interval [LOWER,UPPER] to the
          set of regions denoted by the symbol ID. Values LOWER
          and UPPER are included into the interval.

          If the set of regions ID doesn't exist, then it is created.
        |};


      def "region-contains" (tuple [sym; int] @-> int)
        (module Lower)
        {|(region-contains ID X) return the region in ID that has X.

          Returns the lower bound of the first region that contains
          value X in the set of regions with the given ID. Returns nil
          otherwise.

          Returns nil if a set with the given ID doesn't exist.
        |};


      def "region-move" (tuple [sym; sym; int] @-> bool)
        (module Move)
        {|(region-move DST SRC P) moves all regions that contain the point
          P from the set SRC to the set DST. Returns nil if SRC didn't
          contain any such region, otherwise returns t.
        |};

      def "region-lower" (tuple [sym; int] @-> int)
        (module Lower)
        {|(region-lower ID X) the lower bound of region that contains X.

        Returns nil if ID doesn't exist or if it doesn't have a region
        that includes X.

        This fucntion is an alias for REGION-CONTAINS.
        See also, REGION-UPPER.
       |};

      def "region-upper" (tuple [sym; int] @-> int)
        (module Upper)
        {|(region-upper ID X) the upper bound of the region that contains X.

        Returns the upper bound of the region that contains point X or
        nil if there is no such region or such set of regions.

        See also, REGION-LOWER.|};

      def "region-count" (tuple [sym] @-> int)
        (module Count)
        {|(region-count ID) the total number of regions in the set ID.

        Counts the number of regions (including intersecting) stored
        in the set of regions referenced by the symbol ID.

        Returns nil if there is no set with the given ID, otherwise
        returns the number of regions in that set.
        |}
    ]
end

let desc =
  "Provides a set of operations to store and manipulate interval \
   trees. The module provides a persistent storage for intervals, \
   denoted in the module as regions, since these intervals \
   often represent memory regions. Intervals are stored in interval \
   sets, that are implemented as efficient interval tree data \
   structures (using AVL trees underneath the hood). Each interval \
   set is denoted with a symbol, and it is possible to create \
   arbitrary number of sets, as well as move regions from one set to \
   another."

let () = Config.manpage [`S "DESCRIPTION"; `P desc]
let () = Config.when_ready @@ fun _ ->
  Primus.Machine.add_component (module Main) [@warning "-D"];
  Primus.Components.register_generic "lisp-regions" (module Main)
    ~package:"bap"
    ~desc
