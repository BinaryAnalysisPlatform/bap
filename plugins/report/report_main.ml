open Core_kernel
open Bap.Std
open Bap_future.Std
open Format

include Self()

type slot = {
  note : string option;
  stage : int option;
  total : int option;
  created : float;
  updated : float;
  active : bool;
}

let interval = 1

type tasks = slot String.Table.t

let ping_frames = "|/-\\|/-\\"

let tasks = String.Table.create ()

let startup = Unix.gettimeofday ()

let rendering = ref false

let time_elapsed base =
  let now = Unix.gettimeofday () in
  Float.(now - base)

let duration x y =
  Int.of_float_unchecked Float.(y - x)


let to_hms t =
  let open Float in
  let minute = 60. in
  let hour = 60. * minute in
  let day = 24. * hour in
  let secs  = mod_float t minute in
  let mins  = mod_float (t - secs) hour in
  let hours = mod_float (t - mins)  day in
  let (%:) x base = to_int (x / base) in
  (hours %: hour, mins %: minute, secs %: 1.)

module Ansi = struct
  let pp_pos ppf (x,y) = fprintf ppf "\027[%i;%iH%!" y x
  let pp_clear ppf = fprintf ppf "\027[2J%!"
  let pp_kill ppf = fprintf ppf "\027[K%!"
  let pp_em ppf = fprintf ppf "\027[1m"
  let pp_reset ppf = fprintf ppf "\027[0m"
end

let task_second {created} =
  duration created @@ Unix.gettimeofday ()



let pp fmt = fprintf err_formatter fmt

let render_duration line d =
  let h,m,s = to_hms d in
  pp "%a%t[%02d:%02d:%02d]" Ansi.pp_pos (1,line+1) Ansi.pp_kill h m s

let render_times line {created; updated; active} =
  if active then pp "%t" Ansi.pp_em;
  render_duration line @@ if active
  then time_elapsed created
  else Float.(updated - created)

let render_ping {created; updated; active} =
  if active
  then pp "( %c )"
      ping_frames.[
        duration created updated mod String.length ping_frames
      ]
  else pp "done"

let render_percentbar s s' =
  let percent = (s+1) * 100 / s' in
  pp "%3d%% (%d/%d)" percent (s+1) s'

let render_stage_only s =
  pp "( %d )" (s+1)

let render_progress = function
  | {total=Some s'; stage=Some s} -> render_percentbar s s'
  | {total=None;    stage=Some s} -> render_stage_only s
  | t -> render_ping t

let render_note = function
  | {note=Some n; active=true} -> pp " - %s" n
  | _ -> ()

let render_level line level _ =
  pp "%a" Ansi.pp_pos (level+12,line+1)

let render_name level path _ =
  pp "%s: " (List.nth_exn path level)

let render_endline _ = pp "%t\n" Ansi.pp_reset

let render_slot line level path slot =
  List.iter ~f:(fun render -> render slot) [
    render_times line;
    render_level line level;
    render_name level path;
    render_progress;
    render_note;
    render_endline;
  ]


let level_of_name = String.count ~f:(Char.equal '/')


let do_render_progress () =
  Hashtbl.keys tasks |>
  List.sort ~compare:String.compare |>
  List.iteri ~f:(fun line name ->
      let level = level_of_name name in
      let path = String.split name ~on:'/' in
      match Hashtbl.find tasks name with
      | None -> ()
      | Some slot -> render_slot line level path slot);
  pp "%!"

let render_progress tick =
  if not rendering.contents then begin
    rendering := true;
    do_render_progress tick;
    rendering := false;
  end

let update_slots = function
  | Event.Log.Progress {task; note; stage; total} ->
    Hashtbl.map_inplace tasks ~f:(fun s -> {s with active=false});
    let now = Unix.gettimeofday () in
    Hashtbl.update tasks task ~f:(function
        | None -> {note; stage; total; created=now; updated=now; active=true}
        | Some slot -> {
            slot with
            stage;
            note = Option.first_some note slot.note;
            total = Option.first_some total slot.total;
            updated = Unix.gettimeofday ();
            active = true
          });
    Hashtbl.mapi_inplace tasks ~f:(fun ~key:name ~data:task' ->
        if String.is_prefix ~prefix:name task && task <> name
        then {task' with updated=now; active=true}
        else task');
    render_progress ();
  | _ -> ()

let sample ~interval stream =
  Stream.parse stream ~init:0. ~f:(fun last_updated _ ->
      let now = Unix.gettimeofday () in
      if duration last_updated now >= interval
      then (Some (), now)
      else (None, last_updated))

let create_majors_stream () =
  let stream,s = Stream.create () in
  let _ = Gc.Expert.Alarm.create (fun () -> Signal.send s ()) in
  stream

let enable () =
  pp "%t%a" Ansi.pp_clear Ansi.pp_pos (1,1);
  Stream.observe Event.stream update_slots;
  let majors = create_majors_stream () in
  let ticks = sample ~interval @@
    Stream.merge majors Event.stream ~f:(fun () _ -> ()) in
  Stream.observe ticks render_progress;
  render_progress ()

let print_events () =
  Stream.observe Event.stream @@ function
  | Event.Log.Progress {task; note; stage; total} ->
    let opt pp_val ppf = Option.iter ~f:(pp_val ppf) in
    let str = opt pp_print_string and int = opt pp_print_int in
    pp "%s, %a, %a, %a@\n%!" task str note int stage int total
  | _ -> ()

let enabled = Config.flag "progress" ~doc:"Show progress bars"
let events = Config.flag "events" ~doc:"Output progress events"
let () =
  Config.when_ready (fun {Config.get=(!!)} ->
      if !!enabled then enable ();
      if !!events then print_events ());
