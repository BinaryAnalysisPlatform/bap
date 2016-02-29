open Core_kernel.Std
open Bap.Std

(** substitute loads with the value of corresponding memory *)
let resolver memory = object
  inherit Bil.mapper as super
  method! map_load ~mem ~addr endian scale =
    let exp = super#map_load ~mem ~addr endian scale in
    match addr with
    | Bil.Int addr ->
      let exp = Memmap.lookup memory addr |> Seq.hd |> function
        | None -> exp
        | Some (mem,_) -> match Memory.get ~scale ~addr mem with
          | Ok w -> Bil.int w
          | _ -> exp in
      exp
    | _ -> exp
end

let main proj = 
  let prog = Project.program proj in
  let memory = Project.memory proj in
  Term.map sub_t prog ~f:(fun sub -> 
      Term.map blk_t sub ~f:(fun blk -> 
          Blk.map_exp blk ~f:(Exp.map (resolver memory)))) |>
  Project.with_program proj

let () =
  Project.register_pass ~name:"resolve-indirects" main
