let doc = "
# DESCRIPTION

Provides the $(b,bap:mark-visited) and $(b,bap:report-visited) components.
"

open Bap_main
open Bap.Std
open Bap_primus.Std
include Self()

module ReportProgress(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  let report (visited,total) =
    report_progress ~stage:(visited-1) ~total ();
    Machine.return ()

  let init () =
    Bap_primus_track_visited.progress >>> report
end


let () = Extension.declare @@ fun _ ->
  Bap_primus_track_visited.init ();
  Primus.Components.register_generic ~package:"bap" "report-visited"
    (module ReportProgress)
    ~desc:"Reports progress when a new basic block is discovered.";
  Primus.Machine.add_component (module ReportProgress) [@warning "-D"];
  Ok ()
