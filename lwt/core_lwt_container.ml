open Core_kernel.Std

module Lift_sequence(M : Monad) = struct
  open M
  type 'a t = 'a Sequence.t

  let of_list = Sequence.of_list

  let foldi t ~init ~f =
    Sequence.delayed_fold t ~init:(0, init)
      ~f:(fun (i, b) a ~k -> f i b a >>= fun b -> k (i + 1, b))
      ~finish:(fun (_, b) -> return b)

  let fold t ~init ~f =
    Sequence.delayed_fold t ~init
      ~f:(fun b a ~k -> f b a >>= k)
      ~finish:return


  let all t =
    fold t ~init:[] ~f:(fun accum d -> d >>| fun a -> a :: accum)
    >>| fun res ->
    Sequence.of_list (List.rev res)

  let all_unit t = fold t ~init:() ~f:(fun () v -> v)

  let rec find_map t ~f =
    match Sequence.next t with
    | None           -> return None
    | Some (v, rest) ->
      f v >>= function
      | None           -> find_map rest ~f
      | Some _ as some -> return some

  let find t ~f =
    find_map t ~f:(fun elt -> f elt >>| fun b -> if b then Some elt else None)

  let maybe_force ?(how = `Sequential) t =
    match how with
    | `Parallel   -> Sequence.force_eagerly t
    | `Sequential -> t

  let iteri ?how t ~f = all_unit (maybe_force ?how (Sequence.mapi t ~f))

  let iter ?how t ~f = iteri ?how t ~f:(fun _ a -> f a)

  let map ?how t ~f = all (maybe_force ?how (Sequence.map t ~f))

  (* [filter_map] is implemented separately from [map] so that we never need to keep a
     long stream of intermediate [None] results in the accumulator, only to later filter
     them all out. *)
  let filter_map ?how t ~f =
    fold (maybe_force ?how (Sequence.map t ~f)) ~init:[] ~f:(fun acc maybe_v ->
        maybe_v
        >>| function
        | None   -> acc
        | Some v -> v :: acc)
    >>| fun s ->
    Sequence.of_list (List.rev s)

  let filter ?how t ~f =
    filter_map ?how t ~f:(fun a ->
        f a
        >>| function
        | true -> Some a
        | false -> None)

  let init ?how n ~f = map ?how (Sequence.init n ~f:Fn.id) ~f
end

module Lift_list(M : Monad) = struct
  module Sequence = Lift_sequence(M)
  open M

  type 'a t = 'a List.t

  let foldi t ~init ~f  =
    Sequence.foldi (Sequence.of_list t) ~init ~f

  let fold t ~init ~f = foldi t ~init ~f:(fun _ a -> f a)

  let seqmap t ~f =
    fold t ~init:[] ~f:(fun bs a -> f a >>| fun b -> b :: bs)
    >>| List.rev

  let all ds = seqmap ds ~f:Fn.id

  let all_unit ds = ignore (fold ds ~init:() ~f:(fun () d -> d))

  let iteri ?(how = `Sequential) t ~f =
    match how with
    | `Parallel -> all_unit (List.mapi t ~f)
    | `Sequential -> foldi t ~init:() ~f:(fun i () x -> f i x)

  let iter ?how t ~f = iteri ?how t ~f:(fun _ a -> f a)

  let map ?(how = `Sequential) t ~f =
    match how with
    | `Parallel -> all (List.map t ~f)
    | `Sequential -> seqmap t ~f

  let init ?how n ~f = map ?how (List.init n ~f:Fn.id) ~f

  let filter ?how t ~f =
    map t ?how ~f
    >>| fun bools ->
    List.rev (List.fold2_exn t bools ~init:[]
                ~f:(fun ac x b -> if b then x :: ac else ac))

  let filter_map ?how t ~f = map t ?how ~f >>| List.filter_opt

  let rec find_map t ~f =
    match t with
    | [] -> return None
    | hd :: tl ->
      f hd >>= function
      | None -> find_map tl ~f
      | Some _ as some -> return some

  let find t ~f =
    find_map t ~f:(fun elt -> f elt >>| fun b -> if b then Some elt
                    else None)

end

module Lift(M:Monad)(T : sig
                       type 'a t
                       val to_sequence : 'a t -> 'a Sequence.t
                       val of_sequence : 'a Sequence.t -> 'a t

                     end) =
struct
  module Seq = Lift_sequence(M)
  open Seq
  open T
  open M

  type 'a monad = 'a M.t
  type 'a t = 'a T.t

  let foldi t ~init ~f = foldi ~init (to_sequence t) ~f
  let fold t  ~init ~f = fold ~init (to_sequence t) ~f
  let all t = Seq.all (to_sequence t) >>| of_sequence
  let all_unit t = all_unit (to_sequence t)
  let iter ?how t ~f = iter ?how (to_sequence t) ~f
  let iteri ?how t ~f = iteri ?how (to_sequence t) ~f
  let map ?how t ~f = Seq.map ?how (to_sequence t) ~f >>| of_sequence

  let init ?how n ~f = Seq.init ?how n ~f >>| of_sequence
  let filter ?how t ~f =
    filter ?how (to_sequence t) ~f >>| of_sequence
  let filter_map ?how t ~f =
    filter_map ?how (to_sequence t) ~f >>| of_sequence
  let find_map t ~f = find_map (to_sequence t) ~f
  let find t ~f = find (to_sequence t) ~f
end
