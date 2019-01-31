open Core_kernel
open Regular.Std
open Bap.Std
include Self()


let () = Config.manpage [
    `S "DESCRIPTION";
    `P
      "Provide a serialization for BIR and BIL, using piqi
      serialization files. The plugin provides five formats
      of serialization: JSON, protobuf, textual, binary and XML.";
    `P
      "The serialization routines can be use programmatically, using
      the Data interface, or from a print plugin";

    `S "SEE ALSO";
    `P "$(b,regular)(3), $(b,bap-plugin-print)(1), $(b,bap-piqi)(3)"
  ]

module Bil = struct
  open Bil_piqi
  open Bir_piqi

  let writer f fmt =
    let to_bytes s = f fmt s |> Bytes.of_string in
    Data.Write.create ~to_bytes ()

  let reader f fmt =
    let of_bytes b = f fmt (Bytes.to_string b) in
    Data.Read.create ~of_bytes ()

  let ver = "0.1"
  let desc_of_type = function
    | `json -> "JSON"
    | `pb -> "protobuf"
    | `piq -> "textual"
    | `pib -> "binary"
    | `xml -> "XML"
  let s fmt =
    sprintf "Piqi generated %s serializer" (desc_of_type fmt)

  let register () =
    List.iter all_of_fmt ~f:(fun fmt ->
        let name = Sexp.to_string (sexp_of_fmt fmt) in
        let desc = s fmt in
        Stmt.add_reader ~desc ~ver name (reader stmt_of_string fmt);
        Stmt.add_writer ~desc ~ver name (writer string_of_stmt fmt);
        Exp.add_reader  ~desc ~ver name (reader exp_of_string fmt);
        Exp.add_writer  ~desc ~ver name (writer string_of_exp fmt);
        Bil.add_reader  ~desc ~ver name (reader bil_of_string fmt);
        Bil.add_writer  ~desc ~ver name (writer string_of_bil fmt);
        Sub.add_writer ~desc ~ver name (writer string_of_sub fmt);
        Sub.add_reader ~desc ~ver name (reader sub_of_string fmt);
        Blk.add_writer ~desc ~ver name (writer string_of_blk fmt);
        Blk.add_reader ~desc ~ver name (reader blk_of_string fmt);
        Arg.add_writer ~desc ~ver name (writer string_of_arg fmt);
        Arg.add_reader ~desc ~ver name (reader arg_of_string fmt);
        Phi.add_writer ~desc ~ver name (writer string_of_phi fmt);
        Phi.add_reader ~desc ~ver name (reader phi_of_string fmt);
        Def.add_writer ~desc ~ver name (writer string_of_def fmt);
        Def.add_reader ~desc ~ver name (reader def_of_string fmt);
        Program.add_writer ~desc ~ver name (writer string_of_program fmt);
        Program.add_reader ~desc ~ver name (reader program_of_string fmt))

end

let () = Config.when_ready @@ fun _ -> Bil.register ()
