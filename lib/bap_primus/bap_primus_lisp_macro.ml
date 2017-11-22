let take_rest xs ys =
  let rec take xs ys zs = match xs,ys with
    | [],[] -> Some zs
    | [x], (_ :: _ :: ys as rest) -> Some ((x,rest)::zs)
    | x :: xs, y :: ys -> take xs ys ((x,[y])::zs)
    | _ :: _, [] | [],_ -> None in
  match take xs ys []with
  | Some [] -> Some (0,[])
  | Some ((z,rest) :: _ as bs) ->
    Some (List.length rest, List.rev bs)
  | None -> None

let bind macro cs = take_rest macro.code.param cs
