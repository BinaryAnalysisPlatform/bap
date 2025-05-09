open Core
open Bap.Std
open Regular.Std
open Bap_knowledge
open Bap_core_theory

let package = "superset-cache-guide"

let knowledge_reader = Data.Read.create
    ~of_bigstring:Knowledge.of_bigstring ()

let knowledge_writer = Data.Write.create
    ~to_bigstring:Knowledge.to_bigstring ()

let knowledge_cache () =
  Data.Cache.Service.request
    knowledge_reader
    knowledge_writer
            
module Cache_metadata = struct
  type t = string String.Map.t [@@deriving sexp, bin_io]
  let equal = String.Map.equal String.equal
  let set = String.Map.set
  let empty = String.Map.empty
  let fold = String.Map.fold
  let length = String.Map.length
end

let cache_metadata_t =
  Knowledge.Domain.optional
    ~inspect:Cache_metadata.sexp_of_t ~equal:Cache_metadata.equal
    "cache_metadata_t"

let cache_persistent =
  Knowledge.Persistent.of_binable
    (module struct
       type t = Cache_metadata.t option [@@deriving bin_io] end)

let digests =
  let attr ty persistent name desc =
    let open Theory.Program in
    Knowledge.Class.property ~package cls name ty
      ~persistent ~public:true ~desc in
  attr cache_metadata_t cache_persistent "cache_metadata_t"
    "Information about what other items can be looked up in the cache"

let load_cache_with_digest cache digest =
  match Data.Cache.load cache digest with
  | None -> false
  | Some state ->
     Toplevel.set state;
     true

let store_knowledge_in_cache digest =
  let digest = digest ~namespace:"knowledge" in
  let cache = knowledge_cache () in
  Toplevel.current () |>
  Data.Cache.save cache digest

let import_knowledge_from_cache digest =
  let cache = knowledge_cache () in
  load_cache_with_digest cache digest

let load_knowledge digest p =
  let digest = digest ~namespace:"knowledge" in
  match p with
  | None -> import_knowledge_from_cache digest
  | Some path when not (Sys.file_exists path) ->
    import_knowledge_from_cache digest
  | Some path ->
    Toplevel.set @@ Knowledge.load path;
    true

let make_digest inputs =
  let inputs = String.concat inputs in
  fun ~namespace ->
    let d = Data.Cache.Digest.create ~namespace in
    Data.Cache.Digest.add d "%s" inputs

let guide = KB.Symbol.intern "cache_map" Theory.Program.cls
let metadata_digest =
  (make_digest [ "superset-cache-metadata" ])

let save () =
  let _ = Toplevel.eval digests guide in
  store_knowledge_in_cache metadata_digest
          
(* Retrieve the metadata of all digests *)
let with_digests f =
  let state = Toplevel.current () in
  let _ = load_knowledge metadata_digest None in
  let ds = Toplevel.eval digests guide in
  let r = f ds in
  Toplevel.set state;
  r
  
let cache_corpus_metrics ds =
  match ds with
  | Some ds ->
     Cache_metadata.fold ds ~init:[] ~f:(fun ~key ~data l ->
         let digest = Data.Cache.Digest.of_string data in
         if import_knowledge_from_cache digest then
           Metrics.get_summary () :: l else l
       )
  | None -> []
