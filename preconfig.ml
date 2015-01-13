let targets = [
  "myocamlbuild.ml.in", "myocamlbuild.ml";
  "setup.ml.in",        "setup.ml";
  "_tags.in",           "_tags";
]

let stop = "OASIS_STOP"

let search_stop dst =
  let epos = String.length dst in
  let slen = String.length stop in
  let rec seek_stop i p =
    if p = epos then raise End_of_file;
    if dst.[p] <> stop.[i] then seek_stop 0 (p+1)
    else if i = slen - 1 then p else seek_stop (i+1) (p+1)in
  let rec seek_newline p =
    if p = epos then raise End_of_file;
    if dst.[p] <> '\n' then seek_newline (p+1) else p + 1 in
  seek_stop 0 0 |> seek_newline

let patch (src_file,dst_file) =
  let src = open_in_bin src_file in
  let patch_len = in_channel_length src in
  let patch = String.make patch_len '\x00' in
  really_input src patch 0 patch_len;
  close_in src;
  let dst = open_in_bin dst_file in
  let dst_len = in_channel_length dst in
  let buf = String.make (patch_len + dst_len) '\x00' in
  really_input dst buf 0 dst_len;
  close_in dst;
  let pos = try search_stop buf with End_of_file -> dst_len in
  String.blit patch 0 buf pos patch_len;
  let len = pos + patch_len in
  let dst = open_out_bin dst_file in
  output dst buf 0 len;
  close_out dst


let () = List.iter patch targets
