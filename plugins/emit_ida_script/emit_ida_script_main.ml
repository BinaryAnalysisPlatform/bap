open Core_kernel.Std
open Bap.Std
open Bap_bundle.Std
include Self()

(** ida uses a strange color coding, bgr, IIRC  *)
let idacode_of_color = function
  | `green -> 0x99ff99
  | `red -> 0xCCCCFF
  | `yellow -> 0xC2FFFF
  | _ -> invalid_arg "unexpected color"

let string_of_color c = Sexp.to_string (sexp_of_color c)

(** Access stored data from the plugin bundle *)
let read_file name =
  let bundle = main_bundle () in
  match Bundle.get_data bundle name with
  | None -> assert false
  | Some s -> s

(** Each function in this module should return a string that should be
    a valid piece of python code. Except for the prologue and epilogue
    all pieces should be independent of each other, so that they can
    be emited to the script in an arbitrary order.

    The emitted code can contain substitutions. Each substitution is a
    string starting with a percent sign followed immediately by an
    alpha numeric sequence, denoting the name, e.g., [$name]. The
    substitution name can be also delimited with curly brackets, e.g.,
    [${name}]. Currently the following substitutions are supported
    (i.e., valid names are):
    "sub_name" - name of a parent subroutine
    "addr" - an address of instruction that produced the term, or
            BADADDR otherwise.*)
module Py = struct
  (** this is emitted at the start of the script *)
  let prologue = read_file "prologue.py"

  (** this is emitted at the end of the script *)
  let epilogue =
    {|
# eplogue code if needed
|}

  let color s = sprintf "SetColor($addr, CIC_ITEM, 0x%06x)" (idacode_of_color s)
  let comment s = sprintf "add_to_comment($addr, '%s', '%s')" (Value.tagname s) (Value.to_string s)
end

(** [emit_attr buffer sub_name insn_addr attr] emits into the [buffer]
    a python code that corresponds to the given attribute [attr], that
    is attached to a term that occurs in the scope of a function with
    a name [sub_name]. If the term is non-artifical, then [insn_addr]
    is an address of a corresponding instruction, otherwise it is
    [None].*)

let emit_attr buf sub_name addr attr =
  let open Value.Match in
  let substitute = function
    | "sub_name" -> sub_name
    | "addr" ->
      Option.value_map
        addr ~f:(fun a -> "0x" ^ Addr.string_of_value a)
        ~default:"BADADDR"
    | s -> s in
  let case tag f = case tag (fun attr ->
      Buffer.add_substitute buf substitute (f attr);
      Buffer.add_char buf '\n') in
  switch attr @@
  case color Py.color @@
  default (fun () ->
      Buffer.add_substitute buf substitute (Py.comment attr);
      Buffer.add_char buf '\n')

let program_visitor buf attrs =
  object
    inherit [string * word option] Term.visitor
    method! leave_term _ t (name,addr) =
      Term.attrs t |> Dict.to_sequence |> Seq.iter ~f:(fun (_,x) ->
          let attr = Value.tagname x in
          if List.mem attrs attr then emit_attr buf name addr x);
      name,addr
    method! enter_term _ t (name,_) =
      name,Term.get_attr t Disasm.insn_addr
    method! enter_sub sub (_,addr) = Sub.name sub,addr
  end

let extract_script data code attrs =
  let open Value.Match in
  let buf = Buffer.create 4096 in
  Buffer.add_string buf Py.prologue;
  Memmap.to_sequence data |> Seq.iter ~f:(fun (mem,x) ->
      switch x @@
      case python (fun line -> Buffer.add_string buf line) @@
      default ignore);
  (program_visitor buf attrs)#run code ("",None) |> ignore;
  Buffer.add_string buf Py.epilogue;
  Buffer.contents buf


let main dst attrs project =
  let data = Project.memory project in
  let code = Project.program project in
  let data = extract_script data code attrs in
  match dst with
  | None -> print_string data
  | Some dst -> Out_channel.write_all dst ~data

module Cmdline = struct
  open Cmdliner
  let man = [
    `S "DESCRIPTION";
    `P "Iterates through memory tagged with text objects that have
      a `python' tag, and dumps them into a python script, that
      can be later loaded into the IDA. "
  ]

  let info = Term.info name ~version ~doc ~man

  let dst : string option Term.t =
    let doc = "Dump annotations to the specified file $(docv). If
            not specified, then the script will dumped into the
            standard output" in
    Arg.(value & opt (some string) None & info ["file"]
           ~doc ~docv:"NAME")

  let attrs =
    let doc = "emit specified BIR attribute" in
    Arg.(value & opt_all string [] & info ["attr"] ~doc)

  let args = Term.(const main $dst $attrs),info
end

let () =
  match Cmdliner.Term.eval ~argv Cmdline.args with
  | `Ok main -> Project.register_pass' main
  | `Help | `Version -> exit 0
  | `Error _ -> exit 1
