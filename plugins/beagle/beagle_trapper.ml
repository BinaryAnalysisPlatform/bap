open Core_kernel.Std
open Format


module type Alphabet = sig
  val length : int
  val index : char -> int
end

module Ascii = struct
  let (-) x y = Char.(to_int x - Char.to_int y)
  module Alpha = struct
    module Caseless = struct
      let length = 26
      let index c = Char.uppercase c - 'A'
    end
    let length = 26 * 2
    let index c =
      if Char.is_uppercase c then c - 'A'
      else c - 'a' + 26
  end

  module Digits = struct
    let length = 10
    let index c = c - '0'
  end

  module Alphanum = struct
    module Caseless = struct
      let length = 26 + 10
      let index c =
        if Char.is_alpha c then Alpha.Caseless.index c
        else Digits.index c + 26
    end
    let length = 26 * 2 + 10
    let index c =
      if Char.is_alpha c then Alpha.index c
      else Digits.index c + 26 * 2
  end

  module Printable = struct
    let length = 95
    let index c =  c - ' '
  end
  let length = 128
  let index = Char.to_int
end

module Make(Alpha : Alphabet) = struct
  type t = {
    dict : t Int.Map.t;
    data : string list;
  } [@@deriving bin_io, compare, sexp]


  let spectrum word =
    Array.init Alpha.length ~f:(fun i ->
        String.count word ~f:(fun c -> Alpha.index c = i))

  let empty = {dict = Int.Map.empty; data=[]}


  let add_word dict word =
    let count = spectrum word in
    let rec add {dict; data} i =
      if i < Array.length count then {
        data;
        dict = Map.update dict count.(i) ~f:(function
            | None -> add empty (i+1)
            | Some sub -> add sub (i+1))
      } else {empty with data = word :: data} in
    add dict 0

  let build dict word =
    let count = spectrum word in
    let rec find {dict; data} i =
      if i < Array.length count then
        Sequence.range 0 count.(i) ~stop:`inclusive |>
        Sequence.concat_map ~f:(fun cnt -> match Map.find dict cnt with
            | None -> Sequence.empty
            | Some dict -> find dict (i+1))
      else Sequence.of_list data in
    find dict 0

  let is_buildable dict word =
    let count = spectrum word in
    let rec find {dict} i =
      i >= Array.length count ||
      Sequence.range 0 count.(i) ~stop:`inclusive |>
      Sequence.exists ~f:(fun cnt -> match Map.find dict cnt with
          | None -> false
          | Some dict -> find dict (i+1)) in
    find dict 0

  let add_from_file dict name =
    In_channel.(with_file name ~f:(fold_lines ~init:dict ~f:add_word))

  let of_file name = add_from_file empty name

  let of_files files =
    List.fold files ~init:empty ~f:add_from_file

end


(* module Test = struct *)
(*   module Dict = Make(Ascii.Printable) *)
(*   let run () = *)
(*     let dict = *)
(*       In_channel.(with_file Sys.argv.(1) *)
(*                     ~f:(fold_lines ~init:Dict.empty ~f:Dict.add_word)) in *)
(*     printf "Dictionary is %d MB big\n" (Dict.bin_size_t dict / 1024 / 1024); *)
(*     let prompt () = *)
(*       printf "Enter characters and hit enter (or Ctrl-D to stop): %!" in *)
(*     prompt (); *)
(*     In_channel.iter_lines stdin ~f:(fun set -> *)
(*         Dict.build dict set |> Sequence.iter ~f:print_endline; *)
(*         prompt ()) *)

(* end *)

(* let () = Test.run () *)
