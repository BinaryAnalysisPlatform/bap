open Core_kernel.Std
open Bap.Std
open Format
include Self()

(** ida uses a strange color coding, bgr, IIRC  *)
let idacode_of_color = function
  | `black   -> 0x000000
  | `red     -> 0xCCCCFF
  | `green   -> 0x99FF99
  | `yellow  -> 0xC2FFFF
  | `blue    -> 0xFFB2B2
  | `magenta -> 0xFFB2FF
  | `cyan    -> 0xFFFFB2
  | `white   -> 0xFFFFFF
  | `gray    -> 0xEAEAEA
  | _ -> invalid_arg "unexpected color"

let string_of_color c = Sexp.to_string (sexp_of_color c)

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
            None otherwise.*)
module Py = struct
  (** this is emitted at the start of the script *)
  let prologue =
    {|
from bap.utils import ida
|}

  (** this is emitted at the end of the script *)
  let epilogue =
    {|
# eplogue code if needed
|}


  let escape = unstage @@ String.Escaping.escape
      ~escapeworthy:['\''; '\\'; '$']
      ~escape_char:'\\'


  let color s = sprintf "ida.set_color($addr, 0x%06x)" (idacode_of_color s)
  let comment s =
    sprintf "ida.comment.add($addr, '%s', '%s')"
      (escape (Value.tagname s)) (escape (Value.to_string s))
  let foreground s =
    sprintf "ida.comment.add($addr, 'foreground', '%s')" (string_of_color s)
  let background s =
    sprintf "ida.comment.add($addr, 'background', '%s')" (string_of_color s)

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
    | "addr" -> asprintf "%a" Word.pp_hex addr
    | s -> s in
  let case tag f = case tag (fun attr ->
      Buffer.add_substitute buf substitute (f attr);
      Buffer.add_char buf '\n') in
  switch attr @@
  case color Py.color @@
  case foreground Py.foreground @@
  case background Py.background @@
  default (fun () ->
      Buffer.add_substitute buf substitute (Py.comment attr);
      Buffer.add_char buf '\n')

let program_visitor buf attrs =
  object
    inherit [string * word option] Term.visitor
    method! leave_term _ t (name,addr) =
      match addr with
      | None -> name,addr
      | Some addr ->
        Term.attrs t |> Dict.to_sequence |> Seq.iter ~f:(fun (_,x) ->
            let attr = Value.tagname x in
            if List.mem ~equal:String.equal attrs attr
            then emit_attr buf name addr x);
        name,Some addr
    method! enter_term _ t (name,_) =
      name,Term.get_attr t address
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

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Iterates through memory for tagged BIR attributes,
      and dumps them into a python script, that can be later
      loaded into IDA. The special `color' tag causes the
      respective address to be colored.";
      `S "SEE ALSO";
      `P "$(b,bap-ida)(3), $(b,bap-plugin-ida)(1)"
    ] in
  let dst = Config.(param (some string) "file" ~docv:"NAME"
                      ~doc:"Dump annotations to the specified file $(docv). If
                            not specified, then the script will dumped into the
                            standard output") in
  let attrs = Config.(param_all string "attr"
                        ~doc: "Emit specified BIR attribute. Can be specified
                               multiple times.") in
  Config.when_ready (fun {Config.get=(!)} ->
      let main = main !dst !attrs in
      Project.register_pass' main )
