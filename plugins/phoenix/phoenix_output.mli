open Core_kernel
open Bap.Std


(** [store ~root syms cfg] store data in a phoenix format in the
    [root] folder.

    [syms] is a symbol table, that drives the overall process. For
    each entry in a table there will be created a corresponding file
    in [cfg], [sacfg], [bil] and [hil] subfolders. Correspondingly,
    if [syms] is empty, nothing will be stored at all.

    [cfg] is a reconstructed cfg stored as a table of blocks.

    @return a path to a root directory of the phoenix folder (if root
    was provided, then it will be returned, otherwise it will be
    mangled from the executable name).
*)
module Make(Env : sig
    val project : project
    val options : Phoenix_options.t
    module Target : Target
  end) : sig
  val store : unit -> string
end
