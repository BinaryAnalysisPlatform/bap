let doc = {|
# DESCRIPTION

Identifies functions that are stubs and redirects calls to stubs to
the calls to the implemenations, in case if the latter is present in
the binary.

A stub is piece of binary code that is used to call a function
implementation. Most commonly stubs are employed for external
functions, whose implementation is later loaded from some library,
however some ABIs are using stubs for internal functions, in case if
they have external linkage.

Usually, it is easy to identify whether a function is a stub or not,
since most of the ABIs put them in a specially named section. Some
of the section names are already known to BAP, but you can specify
more with using the $(b,--stub-resolver-names) parameter.

However, some architectures, in particular PowerPC are storing stub
functions directly in the text section with other code without any
indicators. To catch them we employ a simple signature matching
approach. The signatures, could be specified in a file, named
$(b,<triple>.stubs), e.g., for $(b,powerpc.stubs) with the following
contents (see the $(b,--stub-resolve-signatures) parameter description
for more details on the format of accepted inputs):

```
; a list of words that commonly start a stub,
; one per line, with `;' for comments.

3d601001 ; lis r11, 4097
3d601002 ; lis r11, 4098
3d601003 ; lis r11, 4099
3d601004 ; lis r11, 4100

```

|}

open Core_kernel
open Bap.Std
open Bap_core_theory
open Bap_knowledge
open Bap_future.Std
open Bap_main
open KB.Syntax

include Loggers()

let known_stub_names = [
  ".plt";
  "__stubs";
  ".MIPS.stubs";
]

let default_signatures_folder = Stub_resolver_config.signatures_path

let names = Extension.Configuration.parameters
    Extension.Type.(list string) "names"
    ~doc:(sprintf "The list of known sections that contain \
                   function stubs. The names specified with this \
                   parameter are appended to the existing list \
                   that includes: %s" @@
          String.concat ~sep:", " @@
          List.map known_stub_names ~f:(sprintf "$(b,%s)"))

let signatures = Extension.Configuration.parameters
    Extension.Type.(list path) "signatures"
    ~doc:("A list of folders and files that contain signatures for \
           stubs identification. Each file shall have a name of the \
           form $(b,<target>.stubs) and contain a list of words each \
           denoting a possible starting sequence of a bytes for a \
           stub. The <target> is the name of the target, e.g., \
           $(b,arm.stubs), $(b,armv7-linux-gnueabi.stubs), etc. Each \
           word denoting a signature must be encoded as an ASCII \
           number and be binary (start with $(b,0b)), octal (start \
           with $(b,0o), or hexadecimal (start with $(b,0x), e.g., \
           $(b,0xDEADBEEF). If the prefix is omitted then the \
           hexadecimal notation is assumed, e.g., $(b,DEADBEEF) is \
           also acceptable. The signature length is automatically \
           inferred from the word, i.e., the leading zeros are not \
           discarded. By default we search in the current working \
           folder and in " ^
          default_signatures_folder)

module Stubs : sig
  type t
  val create : ctxt -> Ogre.doc -> t
  val mem : t -> Bitvec.t -> bool
end = struct
  open Image.Scheme
  open Ogre.Syntax
  type t = {lower : Bitvec.t; upper : Bitvec.t}


  let empty = {lower = Bitvec.zero; upper = Bitvec.zero}

  let width = Ogre.(require Image.Scheme.bits >>| Int64.to_int_trunc)

  let find stubs =
    width >>= fun width ->
    let module Addr = Bitvec.Make(struct
        let modulus = Bitvec.modulus width
      end) in
    Ogre.request named_region ~that:(fun {info=name} ->
        Set.mem stubs name) >>| function
    | Some {addr; size} -> {
        lower = Addr.int64 addr;
        upper = Addr.(int64 addr + int64 size)
      }
    | None -> empty

  let mem {lower; upper} addr =
    Bitvec.(lower <= addr) &&
    Bitvec.(upper > addr)

  let stubs ctxt =
    List.fold (Extension.Configuration.get ctxt names)
      ~init:(Set.of_list (module String) known_stub_names)
      ~f:(fun init -> List.fold ~init ~f:Set.add)

  let create ctxt doc = match Ogre.eval (find (stubs ctxt)) doc with
    | Ok plt -> plt
    | Error err ->
      warning "failed to find plt entries: %a" Error.pp err;
      empty
end

module Signatures : sig
  type t
  val collect : ctxt -> t
  val matching : Theory.Target.t -> t -> Word.Set.t
end = struct
  type parser_outcome =
    | Success of word
    | Failure of string
    | Empty
    | Comment

  type t = (string * Word.Set.t) list

  let is_prefixed s =
    String.length s > 1 && match s.[0],s.[1] with
    | '0',('b'|'o'|'x') -> true
    | _ -> false

  let prepend_0x s = if is_prefixed s then s else "0x"^s

  let parse_word s =
    let s = prepend_0x s in
    let n = String.length s - 2 in
    Word.create (Bitvec.of_string s) @@ match s.[0], s.[1] with
    | '0','b' -> n
    | '0','o' -> n * 3
    | '0','x' -> n * 4
    | _ -> assert false

  let parse_line s = match String.strip s with
    | "" -> Empty
    | s when s.[0] = ';' -> Comment
    | s when String.for_all s ~f:Char.is_whitespace -> Empty
    | s ->
      let s = String.strip @@
        List.hd_exn (String.split s ~on:';') in
      try Success (parse_word s)
      with Invalid_argument err -> Failure err

  let parse_file file =
    In_channel.read_lines file |>
    List.foldi ~init:Word.Set.empty ~f:(fun number words line ->
        match parse_line line with
        | Empty | Comment -> words
        | Success word -> Set.add words word
        | Failure msg ->
          error "File %S, line %d:@\n\
                 Failed to parse the stub signature:@\n%s"
            file (number+1) msg;
          words)

  let parse_filename s =
    match String.split (Filename.basename s) ~on:'.' with
    | [""; "stubs"] -> None      (* for .stubs *)
    | [name; "stubs"] -> Some name
    | _ -> None

  let collect ctxt : t =
    let signatures = Extension.Configuration.get ctxt signatures in
    let paths = Filename.current_dir_name ::
                default_signatures_folder ::
                List.concat signatures in
    let add_file sigs path = match parse_filename path with
      | None -> sigs
      | Some triple ->
        (triple, parse_file path) :: sigs in
    let add_files sigs folder =
      try Array.fold (Sys.readdir folder) ~init:sigs ~f:(fun sigs path ->
          add_file sigs (Filename.concat folder path))
      with _ -> sigs in
    List.fold paths ~init:[] ~f:(fun sigs path ->
        if Sys.file_exists path && Sys.is_directory path
        then add_files sigs path
        else add_file sigs path)


  let matching target sigs =
    Word.Set.union_list @@
    List.filter_map sigs ~f:(fun (t' ,s) ->
        Option.some_if (Theory.Target.matches target t') s)

end

let mark_plt_as_stub ctxt : unit =
  KB.Rule.(declare ~package:"bap" "stub-resolver" |>
           dynamic ["code"] |>
           dynamic ["stub-resolver:names"] |>
           require Theory.Label.addr |>
           require Theory.Label.unit |>
           require Theory.Unit.path |>
           provide (Value.Tag.slot Sub.stub) |>
           comment "marks code in the specially named sections as stubs");
  Project.Info.(Stream.(observe @@ zip file spec)) @@ fun (file,spec) ->
  let stubs = Stubs.create ctxt spec in
  KB.promise (Value.Tag.slot Sub.stub) @@ fun label ->
  KB.collect Theory.Label.unit label >>=? fun unit ->
  KB.collect Theory.Unit.path unit >>=? fun path ->
  KB.collect Theory.Label.addr label >>|? fun addr ->
  Option.some_if (path = file && Stubs.mem stubs addr) ()


let bitvec_of_memory mem =
  Bitvec.of_binary @@
  String.rev @@
  Bigsubstring.to_string (Memory.to_buffer mem)

let word_of_memory mem =
  let width = Memory.length mem * 8 in
  Word.create (bitvec_of_memory mem) width



let with_path_and_unit label f =
  KB.collect Theory.Label.unit label >>=? fun unit ->
  KB.collect Theory.Unit.path unit >>=? fun path ->
  f path unit

let find_mem target code addr =
  let addr = Word.code_addr target addr in
  Memmap.lookup code addr |>
  Seq.find_map ~f:(fun (mem,_) ->
      match Memory.view ~from:addr mem with
      | Ok view -> Some view
      | Error _ -> None)

let detect_stubs_by_signatures ctxt : unit =
  KB.Rule.(declare ~package:"bap" "stub-detector" |>
           dynamic ["code"] |>
           dynamic ["stub-resolver:signatures"] |>
           require Theory.Label.addr |>
           require Theory.Label.unit |>
           require Theory.Unit.path |>
           provide (Value.Tag.slot Sub.stub) |>
           comment "marks bytes sequences that match signatures as stubs");
  let matches sigs mem =
    let mem = word_of_memory mem in
    Set.mem sigs mem ||
    Set.exists sigs ~f:(fun s ->
        Word.bitwidth s < Word.bitwidth mem &&
        Word.equal s @@
        Word.extract_exn mem
          ~lo:(Word.bitwidth mem - Word.bitwidth s)) in
  let sigs = Signatures.collect ctxt in
  Project.Info.(Stream.(observe @@ zip file code)) @@ fun (file,code) ->
  KB.promise (Value.Tag.slot Sub.stub) @@ fun label ->
  KB.collect Theory.Label.addr label >>=? fun addr ->
  with_path_and_unit label @@ fun path unit ->
  KB.collect Theory.Unit.target unit >>| fun target ->
  Option.bind (find_mem target code addr) ~f:(fun mem ->
      let sigs = Signatures.matching target sigs in
      Option.some_if (path = file && matches sigs mem) ())

let update prog =
  let links = Stub_resolver.run prog in
  (object inherit Term.mapper
    method! map_jmp jmp =
      match Jmp.alt jmp with
      | None -> jmp
      | Some alt -> match Jmp.resolve alt with
        | Second _ -> jmp
        | First tid -> match Map.find links tid with
          | Some tid' ->
            Jmp.with_alt jmp (Some (Jmp.resolved tid'))
          | _ -> jmp
  end)#run prog

let main = Project.map_program ~f:update

let () = Extension.declare ~doc @@ fun ctxt ->
  Bap_abi.register_pass main;
  mark_plt_as_stub ctxt;
  detect_stubs_by_signatures ctxt;
  Ok ()
