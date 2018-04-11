open Core_kernel
open Regular.Std
open Bap.Std
open Bap_strings.Std

open Format

include Self()

let min_length = Config.(param int) "min-len"
    ~default:4
    ~synonyms:["bytes"]
    ~doc:"ignore strings that has length less then $(docv)"

let address = Config.flag "print-address"
    ~doc:"prints an address of each found string"

let scan (mem,value) =
  let read n = match Memory.get ~disp:n mem with
    | Error _ -> None
    | Ok c -> match Word.to_int c with
      | Error _ -> assert false
      | Ok n -> Some (Char.of_int_exn n) in
  Strings.Scanner.run ~read 0 |>
  Seq.fold ~init:Addr.Map.empty ~f:(fun strs (off,str) ->
      Map.add strs
      ~key:(Addr.nsucc (Memory.min_addr mem) off)
      ~data:str)


let union = Map.merge ~f:(fun ~key -> function
    | `Both (s1,s2) ->
      Option.some @@
      if String.length s1 > String.length s2 then s1 else s2
    | `Left s | `Right s -> Some s)


let collect ~min_length proj =
  let ms = Project.memory proj |> Memmap.to_sequence in
  Seq.(ms >>| scan |> reduce ~f:union) |> function
  | None -> Addr.Map.empty
  | Some strs -> Map.filteri strs ~f:(fun ~key ~data ->
      String.length data >= min_length)

let escape = String.concat_map ~f:(function
    | '\n' -> "\\n"
    | '\t' -> "\\t"
    | '\r' -> "\\r"
    | c -> String.of_char c)

let print_str with_address =
  Map.iteri ~f:(fun ~key:addr ~data:str ->
      let str = escape str in
      if with_address
      then printf "%s: %s@\n" (Addr.string_of_value addr) str
      else printf "%s@\n" str)

let collect {Config.get=(!!)} proj =
  collect ~min_length:!!min_length proj |>
  Project.set proj Beagle_prey.statics

let print {Config.get=(!!)} proj =
  Option.iter (Project.get proj Beagle_prey.statics)
    ~f:(print_str !!address)

let () = Config.when_ready (fun cfg ->
  Project.register_pass  ~name:"collect" (collect cfg);
  Project.register_pass' ~deps:["strings-collect"] (print cfg))
