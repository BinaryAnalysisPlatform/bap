open Core_kernel.Std
open Or_error

let eval_exn str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase false Format.err_formatter phrase

let eval str = try_with (fun () -> eval_exn str)
