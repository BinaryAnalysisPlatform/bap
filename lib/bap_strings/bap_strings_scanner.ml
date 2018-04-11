open Core_kernel

module Seq = Sequence

type data = {len: int; chars: char list}

let make_string {len; chars} =
  let bytes = String.create len in
  List.iteri chars ~f:(fun i c -> bytes.[len-i-1] <- c);
  bytes

let is_printable ch = Char.(is_print ch || is_whitespace ch)

let next ?(is_stop=Fn.non is_printable) ~read off =
  let rec next {len; chars} =
    match read (off + len) with
    | None -> {len;chars}
    | Some c when is_stop c -> {len; chars}
    | Some c -> next {len = len+1; chars = c :: chars} in
  make_string (next {len=0; chars=[]})


let run ?is_stop ~read off =
  Seq.unfold_step ~init:off ~f:(fun off ->
      let str = next ?is_stop ~read off in
      let len = String.length str in
      if len <> 0
      then Seq.Step.Yield ((off,str),(off+len))
      else match read off with
        | None -> Seq.Step.Done
        | Some _ -> Seq.Step.Skip (off+1))
