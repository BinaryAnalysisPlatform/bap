open Core_kernel.Std
open Bap.Std
open Format


let read arch ic : (string * addr * addr) list =
  let sym_of_sexp x = <:of_sexp<string * int64 * int64>> x in
  let addr_of_int64 x =
    let width = Arch.addr_size arch |> Size.in_bits in
    Addr.of_int64 ~width x in
  List.(Sexp.input_sexps ic >>| sym_of_sexp >>| (fun (s, es, ef) ->
      s, addr_of_int64 es, addr_of_int64 ef))

let read_addrs ic : addr list =
  List.t_of_sexp Addr.t_of_sexp @@ Sexp.input_sexp ic

let write_addrs oc (addrs : addr list) : unit =
  Sexp.output oc @@ List.sexp_of_t Addr.sexp_of_t addrs

let write oc (syms : (string * addr * addr) list) : unit =
  let sexp_of_sym x = <:sexp_of<string * int64 * int64>> x in
  try
    let syms = List.map syms ~f:(fun (s, es, ef) -> s, Addr.to_int64 es |> ok_exn,
                                                    Addr.to_int64 ef |> ok_exn) in
    List.iter syms ~f:(fun sym -> Sexp.output_hum oc @@ sexp_of_sym sym;
                        output_char oc '\n')
  with exn ->
    printf "Output error: %a." Exn.pp exn;
    ()
