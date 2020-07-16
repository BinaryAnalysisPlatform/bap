let doc = {|
# DESCRIPTION

Compares projects by their callgraphs. Outputs differences immitating
uniform diff format, e.g.,

```
- src -> dst
```

indicates that in the alternative version there is no call from src to dst,
and

```
+ src -> dst
```

means that in the base there is no call from src to dst.
|}

open Core_kernel
open Bap.Std
open Graphlib.Std
open Format
open Bap_main

open Extension.Syntax

module G = Graphs.Callgraph

include struct
  open Extension.Configuration
  open Extension.Type

  let ignore_reserved = flag "ignore-reserved"
      ~doc:"Ignore functions that have reserved names, \
            i.e., starting with $(b,_) (underscore) or $(b,.) (dot)."

  let ignore_unresolved = flag "ignore-unresolved"
      ~doc:"Ignore functions with dummy names, i.e., starting with $(b,sub_)."

  let ignore_matching = parameter (some ("REGEXP" %: string)) "ignore-matching"
      ~doc:"Ignore functions that partially match the speciefied PCRE \
            regular expression."

  let output = parameter (some ("FILE" %: string)) "output"
      ~doc:"Outputs all information into the specified file."
end


let name_of_dst jmp = match Jmp.alt jmp with
  | None -> None
  | Some dst -> match Jmp.resolve dst with
    | Second _ -> None
    | First tid ->
      Option.some @@
      String.lstrip ~drop:(function '@' | '%' -> true | _ -> false) @@
      Tid.name tid


let () = Extension.declare ~provides:["collator"] ~doc @@ fun ctxt ->
  let ppf,close = match ctxt-->output with
    | None -> std_formatter, ignore
    | Some file ->
      let chan = Out_channel.create file in
      formatter_of_out_channel chan, fun () -> Out_channel.close chan in
  let match_prefix flag prefix = if ctxt-->flag
    then String.is_prefix ~prefix
    else Fn.const false in
  let match_custom = match ctxt-->ignore_matching with
    | None -> Fn.const false
    | Some regexp ->
      Re.execp @@ Re.compile @@ Re.Pcre.re regexp in
  let matchers = [
    match_prefix ignore_reserved "_";
    match_prefix ignore_reserved ".";
    match_prefix ignore_unresolved "sub_";
    match_custom;
  ] in
  let filter s = not@@List.exists matchers ~f:(fun f -> f s) in
  let call_collector = object
    inherit [string * String.Set.t String.Map.t] Term.visitor
    method! enter_sub sub (_,calls) =
      Sub.name sub,calls
    method! leave_sub _ (_,calls) = "",calls
    method! enter_jmp jmp (src,calls) =
      match name_of_dst jmp with
      | Some dst when filter src && filter dst ->
        src,Map.update calls src ~f:(function
            | None -> String.Set.singleton dst
            | Some dsts -> Set.add dsts dst)
      | _ -> src,calls
  end in

  let calls_of_project proj =
    let prog = Project.program proj in
    snd @@ call_collector#run prog ("",String.Map.empty) in

  let prepare proj =
    calls_of_project proj,0 in

  let print_diffs pref src dsts =
    Set.iter dsts ~f:(fprintf ppf "%c %s -> %s@\n" pref src) in

  let collate ver (base,diffs) proj =
    let alt = calls_of_project proj in
    let diff = Map.symmetric_diff base alt
        ~data_equal:String.Set.equal in
    let is_different = not (Seq.is_empty diff) in
    if is_different
    then fprintf ppf "Version %d differs:@\n%!" ver
    else fprintf ppf "Version %d is not different@\n%!" ver;
    Seq.iter diff ~f:(fun (src,diff) -> match diff with
        | `Left dsts -> print_diffs '-' src dsts
        | `Right dsts -> print_diffs '+' src dsts
        | `Unequal (lhs,rhs) ->
          print_diffs '-' src (Set.diff lhs rhs);
          print_diffs '+' src (Set.diff rhs lhs));
    base,if is_different then diffs + 1 else diffs in

  let print_summary = function
    | 0 -> fprintf ppf "All versions have the same callgraph as the base version\n%!"
    | 1 -> fprintf ppf "Found one version that is different\n%!"
    | n -> fprintf ppf "Found %d versions that are different\n%!" n in


  let summary (_,diff) =
    print_summary diff;
    close () in

  Project.Collator.register ~package:"bap" "callgraph"
    ~prepare ~collate ~summary
    ~desc:"compares binaries by their callgraphs";

  Ok ()
