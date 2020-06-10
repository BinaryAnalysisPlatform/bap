open Core_kernel
open Bap.Std
open Graphlib.Std
open Format
open Bap_main

module G = Graphs.Callgraph


let call_collector = object
  inherit [string * String.Set.t String.Map.t] Term.visitor
  method! enter_sub sub (_,calls) = Sub.name sub,calls
  method! enter_jmp jmp (src,calls) = match Jmp.alt jmp with
    | None -> src,calls
    | Some dst -> match Jmp.resolve dst with
      | Second _ -> src,calls
      | First tid ->
        let dst = Tid.name tid in
        src,Map.update calls dst ~f:(function
            | None -> String.Set.singleton dst
            | Some dsts -> Set.add dsts dst)
end

let calls_of_project proj =
  let prog = Project.program proj in
  snd @@ call_collector#run prog ("",String.Map.empty)

let prepare proj =
  calls_of_project proj,0

let print_diffs pref src dsts =
  Set.iter dsts ~f:(printf "%c %s -> %s@\n" pref src)

let collate ver (base,diffs) proj =
  let diff = Map.symmetric_diff base (calls_of_project proj)
      ~data_equal:String.Set.equal in
  let is_different = not (Seq.is_empty diff) in
  if is_different
  then printf "Version %d differs:@\n%!" ver
  else printf "Version %d is not different@\n%!" ver;
  Seq.iter diff ~f:(fun (src,diff) -> match diff with
      | `Left dsts -> print_diffs '-' src dsts
      | `Right dsts -> print_diffs '+' src dsts
      | `Unequal (lhs,rhs) ->
        print_diffs '-' src (Set.diff lhs rhs);
        print_diffs '+' src (Set.diff rhs lhs));
  base,if is_different then diffs + 1 else diffs


let summary (_,diffs) =
  printf "Found %d versions different from the base version@\n%!"
    diffs

let () = Extension.declare ~provides:["collator"] @@ fun _ctxt ->
  Project.Collator.register ~package:"bap" "callgraph"
    ~prepare ~collate ~summary;
  Ok ()
