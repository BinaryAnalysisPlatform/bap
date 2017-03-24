(* probably we should move this into a separate plugin/pass *)
module Strings = struct
  type state =
    | String of addr * int * char list
    | Data

  let to_ascii word = match Word.to_int word with
    | Error _ -> assert false
    | Ok n -> match Char.of_int_exn n with
      | '\x00' -> Some '\x00'
      | ch when Char.is_print ch || Char.is_whitespace ch -> Some ch
      | _ -> None

  let make_string len chars =
    let bytes = Bytes.create len in
    List.iteri chars ~f:(fun i c -> bytes.[len-i-1] <- c);
    Bytes.to_string bytes

  let scan (mem,sec) =
    let addr = Memory.min_addr mem in
    let rec next strings state disp =
      match Memory.get mem ~disp ~addr with
      | Error _ -> strings
      | Ok byte -> match to_ascii byte,state with
        | None,_ -> next strings Data (disp+1)
        | Some '\x00',Data -> next strings Data (disp+1)
        | Some '\x00',String (_,len,_) when len < 4 ->
          next strings Data (disp+1)
        | Some '\x00',String (base,len,chars) ->
          let data = make_string len chars in
          next (Map.add strings ~key:base ~data) Data (disp+1)
        | Some ch,Data ->
          let base = Addr.nsucc addr disp in
          next strings (String (base,1,[ch])) (disp+1)
        | Some ch,String (base,n,chars) ->
          next strings (String (base,n+1,ch::chars)) (disp+1) in
    next Addr.Map.empty Data 0

  let union = Map.merge ~f:(fun ~key -> function
      | `Both (s1,s2) ->
        Option.some @@
        if String.length s1 > String.length s2 then s1 else s2
      | `Left s | `Right s -> Some s)

  let extract memmap =
    let ms = Memmap.to_sequence memmap in
    Seq.(ms >>| scan |> reduce ~f:union) |> function
    | None -> Addr.Map.empty
    | Some m -> m

end
