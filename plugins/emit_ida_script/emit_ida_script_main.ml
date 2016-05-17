open Core_kernel.Std
open Bap.Std
include Self()

(** ida uses a strange color coding, bgr, IIRC  *)
let idacode_of_color = function
  | `green -> 0x99ff99
  | `red -> 0xCCCCFF
  | `yellow -> 0xC2FFFF
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
            BADADDR otherwise.*)
module Py = struct
  (** this is emitted at the start of the script *)
  let prologue =
    {|
from idautils import *

def sexp2list(s):
    sexp = [[]]
    word = ''
    for c in s:
        if c == '(':
            sexp.append([])
        elif c == ')':
            if word:
                sexp[-1].append(word)
                word = ''
            temp = sexp.pop()
            sexp[-1].append(temp)
        elif c == ' ':
            if word:
                sexp[-1].append(word)
            word = ''
        else:
            word += c
    return sexp[0]

def list2sexp(l):
    if isinstance(l, str):
        return l
    return '(' + ' '.join(list2sexp(e) for e in l) + ')'

def add_to_comment_string(comm, key, value):
    if '(BAP ' in comm:
        start_loc = comm.index('(BAP ')
        bracket_count = 0
        for i in range(start_loc, len(comm)):
            if comm[i] == '(':
                bracket_count += 1
            elif comm[i] == ')':
                bracket_count -= 1
                if bracket_count == 0:
                    end_loc = i + 1
                    BAP_dict = comm[start_loc:end_loc]
                    break
        else:
            # Invalid bracketing.
            # Someone messed up the dict.
            # Correct by inserting enough close brackets.
            end_loc = len(comm)
            BAP_dict = comm[start_loc:end_loc] + (')' * bracket_count)
    else:
        start_loc = len(comm)
        end_loc = len(comm)
        BAP_dict = '(BAP )'

    kv = ['BAP', [key,value]]
    for e in sexp2list(BAP_dict[5:-1]): # Remove outermost '(BAP', ')'
        if isinstance(e, list) and len(e) == 2: # It is of the '(k v)' type
            if e[0] != key: # Don't append if same as required key
                kv.append(e)
        else:
            kv.append(e)

    return comm[:start_loc] + list2sexp(kv) + comm[end_loc:]

def add_to_comment(ea, key, value):
    old_comm = GetCommentEx(ea, repeatable=0)
    if old_comm is None:
        old_comm = ''
    new_comm = add_to_comment_string(old_comm, key, value)
    MakeComm(ea, new_comm)

Wait()
|}

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

let program_visitor buf =
  object
    inherit [string * word option] Term.visitor
    method! leave_term _ t (name,addr) =
      Term.attrs t |> Dict.to_sequence |> Seq.iter ~f:(fun (_,x) ->
          emit_attr buf name addr x);
      name,addr
    method! enter_term _ t (name,_) =
      name,Term.get_attr t Disasm.insn_addr
    method! enter_sub sub (_,addr) = Sub.name sub,addr
  end

let extract_script data code =
  let open Value.Match in
  let buf = Buffer.create 4096 in
  Buffer.add_string buf Py.prologue;
  Memmap.to_sequence data |> Seq.iter ~f:(fun (mem,x) ->
      switch x @@
      case python (fun line -> Buffer.add_string buf line) @@
      default ignore);
  (program_visitor buf)#run code ("",None) |> ignore;
  Buffer.add_string buf Py.epilogue;
  Buffer.contents buf


let main dst project =
  let data = Project.memory project in
  let code = Project.program project in
  let data = extract_script data code in
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

  let args = Term.(const main $dst),info
end

let () =
  match Cmdliner.Term.eval ~argv Cmdline.args with
  | `Ok main -> Project.register_pass' main
  | `Help | `Version -> exit 0
  | `Error _ -> exit 1
