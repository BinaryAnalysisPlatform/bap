open Core_kernel.Std
open Regular.Std
open Bap.Std

module Bil = struct
  open Bil_piqi
  open Bir_piqi

  let writer f fmt = Data.Write.create ~to_bytes:(f fmt) ()

  let reader f fmt = Data.Read.create ~of_bytes:(f fmt) ()

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

let () = Bil.register ()
