open Core_kernel
open Bap.Std
open Bil.Types

let run bil =
  let (--) = Set.remove in
  let (++) = Set.union in
  let free = Exp.free_vars in
  let is_dead var live =
    Var.is_virtual var && not (Set.mem live var) in
  let rec loop init bil =
    List.fold (List.rev bil) ~init:([], init)
      ~f:(fun (bil, live) s -> match s with
          | Special _ | CpuExn _ -> s :: bil, live
          | Jmp e -> s :: bil, free e ++ live
          | If (cond, yes, no) ->
            let yes, live' = loop live yes in
            let no, live'' = loop live no in
            let live = free cond ++ live' ++ live'' in
            Bil.if_ cond yes no :: bil, live
          | Move (var,e) ->
            if is_dead var live then bil, live
            else s :: bil, free e ++ (live -- var)
          | While (cond, body) ->
            let body, live' = loop live body in
            let live = free cond ++ live' in
            Bil.while_ cond body :: bil, live) in
  fst (loop Var.Set.empty bil)
