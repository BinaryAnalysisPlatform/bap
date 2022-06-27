let doc = {|
# DESCRIPTION

Identifies function starts using a predefined sets of function start
signatures. Each signature is a sequence of bytes equipped with a
sample probability of occuring it as a function start. The input
memory is scanned, and for each byte that is not yet classified as a
function start the longest sequence of bytes is searched in the
signatures. If one is found, then the $(b,threshold) parameter defines
the decision procedure. If it is a value below $(b,1.0) then the
sequence of bytes will be classified as a function start if the the
associated probability is higher than the specified threshold.  If the
threshold is greater or equal than 1.0, then the sequence of bytes
will be classified as a function start if the Bayes factor of the two
competing hypothesis is greater than the specified threshold. The
Bayes factor is the ratio between the posterior probabilities of the
competing hypothesis. Therefore, it includes the prior odds of finding
a function start, which makes the hypothesis testing more robust. The
Bayes factor value is having the following interpretations:

```
    Bayes Factor          Strength

    1 to 3.2              Weak
    3.2 to 10             Substantial
    10 to 100             Strong
    100 and greater       Decisive;
```

This plugin is a partial implementation of the starts, partially
BYTEWEIGHT algorithm as described in [1]. Only the byte level matching
is implemented. The $(b,SEE ALSO) section contains links to other
plugins, that provide function identification services.

[1]: Bao, Tiffany, et al. "Byteweight: Learning to recognize
functions in binary code." 23rd USENIX Security Symposium (USENIX
Security 14). 2014.

# SEE ALSO

$(b,bap-byteweight)(1), $(b,bap-plugin-ida)(1), $(b,bap-plugin-read-symbols)(1)
|}

open Core_kernel[@@warning "-D"]
open Bap_main
open Bap_core_theory
open Bap.Std
module Sys = Caml.Sys

open KB.Syntax
include Loggers()

module BW = Bap_byteweight.Bytes
module Sigs = Bap_byteweight_signatures

let p1 m n = float m /. float (m + n)
and p0 m n = float n /. float (m + n)

let roots = KB.Class.property Theory.Unit.cls "byteweight-roots"
    ~package:"bap" @@ KB.Domain.powerset (module Bitvec_order) "roots"

let no_roots = Set.empty (module Bitvec_order)
let of_addrs =
  List.fold ~init:no_roots ~f:(fun roots addr ->
      Set.add roots (Addr.to_bitvec addr))

let make_compiler compiler unit = match compiler with
  | Some name -> KB.return @@ Some (Theory.Compiler.create name)
  | None -> unit-->Theory.Unit.compiler

let compute_root_table path min_length max_length threshold compiler =
  KB.Rule.(begin
      declare ~package:"bap" "precompute-byteweight-rules" |>
      dynamic ["byteweight signatures"] |>
      require Theory.Unit.target |>
      require Project.memory_slot |>
      require Theory.Unit.compiler |>
      provide roots |>
      comment "precomputes byteweight roots"
    end);
  let paths = Option.value_map path ~f:List.return ~default:[] in
  KB.promise roots @@ fun unit ->
  let* target = unit-->Theory.Unit.target in
  let* memory = unit-->Project.memory_slot in
  let* compiler = make_compiler compiler unit in
  KB.guard (not (Memmap.is_empty memory)) >>| fun () ->
  match Sigs.lookup ~paths ?compiler target BW.t with
  | Error `No_signatures ->
    warning "The signatures database is empty.";
    info "install the signatures with `opam install bap-signatures'";
    info "alternatively use `bap-byteweight update'";
    no_roots
  | Error `No_entry s ->
    info "no signatures for %s" s;
    info "use `bap-byteweight train' to create signatures";
    no_roots
  | Error (`Sys_error _ | `Corrupted _ as problem)  ->
    error "the signatures database is broken: %s"
      (Sigs.string_of_error problem);
    no_roots
  | Ok sigs ->
    let find = if Float.(threshold >= 1.0)
      then BW.find_using_bayes_factor
          sigs ~min_length ~max_length threshold
      else BW.find_using_threshold
          sigs ~min_length ~max_length threshold in
    Memmap.to_sequence memory |>
    Seq.fold ~init:no_roots ~f:(fun roots (mem,_) ->
        Set.union roots @@ of_addrs (find mem))

let provide_roots () =
  KB.Rule.(begin
      declare ~package:"bap" "byteweight" |>
      require roots |>
      provide Theory.Label.is_subroutine |>
      comment "uses byteweight to find function starts"
    end);
  KB.promise Theory.Label.is_subroutine @@ fun program ->
  let*? unit = program-->Theory.Label.unit in
  let*? addr = program-->Theory.Label.addr in
  let+ roots = unit-->roots in
  Option.some_if (Set.mem roots addr) true

let enable path min_length max_length threshold comp =
  compute_root_table path min_length max_length threshold comp;
  provide_roots ()

let () =
  let open Extension.Configuration in
  let open Extension.Type in
  let min_length = parameter (int =? 8) "min-length"
      ~doc:"The minimum length of a word, that could identify a \
            function start. Any signatures that are below that \
            length, will not be considered, affect prior \
            probabilities, etc." in
  let max_length = parameter (int =? 16) "max-length"
      ~aliases:["length"]
      ~doc:"The maximum length of a word, that could identify a \
            function start. Any signatures that are greater than that \
            length, will not be considered, affect prior \
            probabilities, etc." in
  let threshold = parameter (float =? 10.) "threshold"
      ~doc:"If greater than 1.0 then it is the Bayes factor, \
            otherwise it is a probability." in
  let sigsfile = parameter (some non_dir_file) "sigs"
      ~aliases:["signatures"]
      ~doc:"Path to the signature file" in
  let compiler = parameter (some string) "compiler"
      ~doc:"Assume the input file is compiled by $(docv)" in
  let enabled = parameter bool ~as_flag:true "enabled"
      ~doc:"Enable/disable byteweight (off by default)" in
  Extension.declare ~doc ~provides:["roots"] @@ fun ctxt ->
  let (!) p = get ctxt p in
  if !enabled
  then enable !sigsfile !min_length !max_length !threshold !compiler;
  Ok ()
