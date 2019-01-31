open Core_kernel
open Or_error
open Bap.Std

type t = [
  | `xF0 (** LOCK                *)
  | `xF2 (** REPNE               *)
  | `xF3 (** REP/REPE            *)
  | `x2E (** CS segment override *)
  | `x36 (** SS segment override *)
  | `x3E (** DS segment override *)
  | `x26 (** ES segment override *)
  | `x64 (** FS segment override *)
  | `x65 (** GS segment override *)
  | `x66 (** operand override    *)
  | `x67 (** address override    *)
] [@@deriving bin_io, sexp, compare, enumerate]

let to_known_prefix = function
  | 0xF0 -> Some `xF0
  | 0xF2 -> Some `xF2
  | 0xF3 -> Some `xF3
  | 0x2E -> Some `x2E
  | 0x36 -> Some `x36
  | 0x3E -> Some `x3E
  | 0x26 -> Some `x26
  | 0x64 -> Some `x64
  | 0x65 -> Some `x65
  | 0x66 -> Some `x66
  | 0x67 -> Some `x67
  | _ -> None

let deref mem =
  Seq.unfold_step ~init:(Memory.min_addr mem)
    ~f:(fun addr ->
        match Memory.get ~addr mem with
        | Ok word -> Seq.Step.Yield (word, Word.succ addr)
        | Error _ -> Seq.Step.Done)

let get memory =
  Seq.(deref memory >>| Word.to_int >>| ok_exn >>| to_known_prefix
       |> take_while ~f:Option.is_some |> filter_opt |> to_list)

let exists pref mem =
  List.mem ~equal:[%compare.equal : t] (get mem) pref

let fold ~init ~f mem = List.fold ~init ~f (get mem)
