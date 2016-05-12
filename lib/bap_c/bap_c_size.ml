open Core_kernel.Std
open Bap.Std
open Bap_c_type
open Bap_c_data

type real_size = [`r32 | `r64 | `r128]
type 'a unqualified = (no_qualifier, 'a) spec

type bits = Int.t

class base (m : model) = object(self)
  method integer (t : integer) : size =
    match m,t with
    | _,#char -> `r8
    | _,#short -> `r16
    | `LP32,#int -> `r16
    | (`ILP32|`LLP64|`LP64),#int -> `r32
    | `ILP64,#int -> `r64
    | (`LP32|`ILP32|`LLP64),#long -> `r32
    | (`ILP64|`LP64), #long -> `r64
    | _,#long_long -> `r64
    | _,`enum x -> self#enum x

  method enum _ = self#integer `uint   (* approximation *)

  method real (v : real) : real_size = match v with
    | `float -> `r32
    | `double -> `r64
    | `long_double -> `r128

  method private double_size : real_size -> size = function
    | `r32 -> `r64
    | `r64 -> `r128
    | `r128 -> `r256

  method complex : complex -> size = function
    | `cfloat -> self#double_size (self#real `float)
    | `cdouble -> self#double_size (self#real `double)
    | `clong_double -> self#double_size (self#real `long_double)

  method floating : floating -> size = function
    | #real as t -> (self#real t :> size)
    | #complex as t -> (self#complex t :> size)

  method basic : basic -> size = function
    | #integer as t -> self#integer t
    | #floating as t -> self#floating t

  method scalar : scalar -> size = function
    | `Basic {Spec.t} -> self#basic t
    | `Pointer _ -> match m with
      | #model32 -> `r32
      | #model64 -> `r64


  method padding t offset =
    let align = self#alignment t in
    (align - offset mod align) mod align

  method alignment t : Int.t =
    let max_align = match m with
      | #model32 -> 32
      | #model64 -> 64 in
    match self#bits t with
    | None -> 0
    | Some width -> min width max_align

  method private padded t = function
    | None -> None
    | Some sz -> Some (sz + self#padding t sz)

  method bits : t -> Int.t option = fun t -> match t with
    | `Void -> None
    | #scalar as t -> Some (Size.in_bits (self#scalar t))
    | `Function _ -> None
    | `Union s -> self#padded t (self#union s)
    | `Array s -> self#array s
    | `Structure s -> self#padded t (self#structure s)


  method array : _  -> Int.t option =
    fun {Spec.t=(t,size)} -> match size with
      | None -> None
      | Some n -> match self#bits t with
        | None -> None
        | Some x -> Some (n * x)

  method union : 'f list unqualified -> Int.t option = fun {Spec.t=fields} ->
    List.map fields ~f:(fun (_,t) -> self#bits t) |> Option.all |> function
    | None -> None
    | Some ss -> List.max_elt ~cmp:Int.compare ss |> function
      | None -> None
      | Some s -> Some s

  method structure : 'f list unqualified -> Int.t option = fun {Spec.t=fields} ->
    List.fold fields ~init:(Some 0) ~f:(fun sz (_,field) -> match sz with
        | None -> None
        | Some sz -> match self#bits field with
          | None -> None
          | Some sz' ->
            let pad = self#padding field sz in
            Some (sz + sz' + pad))
end
