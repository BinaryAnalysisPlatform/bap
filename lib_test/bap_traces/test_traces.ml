open OUnit2
open Core_kernel
open Or_error

open Bap.Std
open Bap_traces.Std
open Event
open Meta

let bed = Word.of_int32 0xBEDl
let coffee = Word.of_int32 0xC0FFEEl
let move = Move.{cell = bed; data = coffee}
let var = Var.create ~fresh:true "temp" (Type.Imm 0xC0FFEE)
let reg = Move.{cell = var; data = coffee}
let bin = Binary.{
    path = "home";
    args = [|"go"|] ;
    envp = [|"drink=coffee"|] ;
    md5sum = ""
  }

let trc = Tracer.{
    name = "coffee";
    args = [|"scent"|] ;
    envp = [|"require=coffee"|] ;
    version="0.1"
  }

let memory_events =
  [ Value.create memory_load move;
    Value.create memory_store move; ]

let test_events =
  memory_events @
  [ Value.create register_read reg;
    Value.create register_write reg; ]

let test_meta =
  let open Dict in
  let meta = set empty binary bin in
  set meta tracer trc

module Tool : Trace.S = struct
  let name = "test_tool"
  let supports = fun tag ->
    let same t = Value.Tag.same tag t in
    same memory_load || same memory_store
end

let test_tool = Trace.register_tool (module Tool)
let empty = Trace.create test_tool

let assert_seq_equal ~ctxt  =
  let cmp s s' = Seq.compare Value.compare s s' = 0 in
  assert_equal ~ctxt ~cmp

let assert_dict_equal ~ctxt =
  let cmp d d' = Dict.compare d d' = 0 in
  assert_equal ~ctxt ~cmp

let unfold_list lst =
  let rest = ref lst in
  fun () -> match !rest with
    | [] -> None
    | e :: es -> rest := es; Some (Ok e)

let new_tracer () =
  Trace.create test_tool (unfold_list test_events)

let save_and_load ctxt =
  let uri = Uri.of_string "file:///tmp/bap_trace.binprot" in
  let save () =
    let t = new_tracer () in
    let t = Trace.set_meta t test_meta in
    let r = Trace.save uri t in
    assert_bool "save failed" (Result.is_ok r) in
  let load () = match Trace.load uri with
    | Ok t ->
      let evs = Trace.read_events t in
      let meta = Trace.meta t in
      let tool = Trace.tool t in
      assert_seq_equal ~ctxt evs (Seq.of_list test_events);
      assert_dict_equal ~ctxt meta test_meta;
      assert_equal ~ctxt test_tool tool
    | Error s -> assert_failure "load failed" in
  save ();
  load ()

let id ctxt =
  let t  = new_tracer () in
  let t' = new_tracer () in
  let id,id' = Trace.(id t, id t') in
  let not_eql = not (Trace.Id.equal id id') in
  assert_bool "id mustn't be equal, failed" not_eql

let trace_tool ctxt =
  let t = new_tracer () in
  let tool = Trace.tool t in
  assert_equal ~ctxt tool test_tool

let set_and_get_attr t attr a ctxt =
  let t = Trace.set_attr t attr a in
  match Trace.get_attr t attr with
  | None -> assert_failure "set/get attribute failed"
  | Some a' -> assert_equal ~ctxt a a'

let has_attr t attr a ctxt =
  let t = Trace.set_attr t attr a in
  let has = Trace.has_attr t attr in
  assert_bool "attribute is absent" has

let set_meta t ctxt =
  let open Dict in
  let t = Trace.set_meta t test_meta in
  let meta = Trace.meta t in
  assert_dict_equal ~ctxt meta test_meta


let find_all_matching ctxt =
  let t = new_tracer () in
  let s = Trace.read_all_matching t
      Value.Match.(begin
          case memory_load  (fun x  -> `Memory) @@
          case memory_store (fun x  -> `Memory) @@
          default           (fun () -> `Unknown)
        end) in
  let s' = Seq.filter s ~f:(fun x -> x = `Memory) in
  assert_bool "failed find_all_matching"
    (Seq.length s' = List.length memory_events)


let supports ctxt =
  let empty = new_tracer () in
  let m = Trace.supports empty memory_load in
  let m' = Trace.supports empty memory_store in
  let r = not (Trace.supports empty register_read) in
  assert_bool "supports failed" (m && m' && r)


let error = Error.of_string "syscall failed"

let events_with_error =
  Error error ::
  List.map test_events ~f:(fun ev -> Ok ev)

let create_with_monitor monitor =
  let unfold_list lst =
    let rest = ref lst in
    fun () -> match !rest with
      | [] -> None
      | e :: es -> rest := es; Some (e) in
  Trace.create ~monitor test_tool (unfold_list events_with_error)

let fail_monitor ctxt =
  let monitor = Trace.Monitor.fail_on_error in
  let t = create_with_monitor monitor in
  let f () = Trace.read_all t memory_load |> Seq.force_eagerly in
  assert_raises (Info.to_exn (Error.to_info error)) f

let miss_monitor ctxt =
  let monitor = Trace.Monitor.ignore_errors in
  let t = create_with_monitor monitor in
  let events = Trace.read_events t |> Seq.force_eagerly in
  assert_seq_equal ~ctxt events (Seq.of_list test_events)

let stop_monitor ctxt =
  let monitor = Trace.Monitor.stop_on_error in
  let t = create_with_monitor monitor in
  let evs = Seq.to_list (Trace.read_events t) in
  assert_equal ~ctxt evs []

let pack_monitor ctxt =
  let pack _ = Value.create memory_load move in
  let monitor = Trace.Monitor.pack_errors pack in
  let t = create_with_monitor monitor in
  let len = Seq.length (Trace.read_events t) in
  let len' = List.length test_events in
  assert_equal ~ctxt len (len' + 1)

let warn_monitor ctxt =
  let a = ref 0 in
  let warn _ = a := !a + 1 in
  let monitor = Trace.Monitor.warn_on_error warn in
  let t = create_with_monitor monitor in
  let s = Trace.read_events t |> Seq.force_eagerly in
  assert_equal ~ctxt !a 1;
  assert_seq_equal ~ctxt s (Seq.of_list test_events)

let user_monitor ctxt =
  let filter _ = `Skip in
  let monitor = Trace.Monitor.create filter in
  let t = create_with_monitor monitor in
  assert_seq_equal ~ctxt (Trace.read_events t) (Seq.of_list test_events)

let empty = new_tracer ()

let suite () =
  "Trace" >:::
  [
    "save_and_load"     >:: save_and_load;
    "id"                >:: id;
    "tool"              >:: trace_tool;
    "set_and_get_attr"  >:: set_and_get_attr empty binary bin;
    "has_attr"          >:: has_attr empty binary bin;
    "set_meta"          >:: set_meta empty;
    "find_all_matching" >:: find_all_matching;
    "supports"          >:: supports;
    "fail_monitor"      >:: fail_monitor;
    "miss_monitor"      >:: miss_monitor;
    "stop_monitor"      >:: stop_monitor;
    "pack_monitor"      >:: pack_monitor;
    "warn_monitor"      >:: warn_monitor;
    "user_monitor"      >:: user_monitor;
  ]
