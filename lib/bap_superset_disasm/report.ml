open Core
open Bap.Std

let () = Random.self_init ()

module Distribution = struct
  type t = {
      (* The ith elem says the number (int) of insns with i
       accumulated total heurism instances *)
      fp_at             : int array;
      tp_at             : int array;
      (* The ith list elem (x, y list) says the number (x) of true
       positives that have i many heurisms in their favor and how
       many false positives (y) they occlude with j number of heurisms. *)
      fp_competitors_at : (int * int array) array;
    } [@@deriving sexp,bin_io]

  let empty threshold =
    let len = threshold + 1 in
    {
      fp_at             = Array.create ~len 0;
      tp_at             = Array.create ~len 0;
      fp_competitors_at = Array.create ~len (0,Array.create ~len 0);
    }

  let add dist tps inst =
    let (p,addr,heurism) = inst in
    let p = min p @@ ((Array.length dist.fp_at) - 1) in
    let insert c =
      let cur = Array.get c p  in
      Array.set c p (cur+1) in
    let _ = 
      if Set.mem tps addr then
        insert dist.tp_at
      else insert dist.fp_at in
    dist
end

let equal _ _ = false

module Cache = struct
  open Bap_knowledge
  open Bap_core_theory
  module SMap = struct
    type t = Distribution.t String.Map.t [@@deriving bin_io,sexp]
    let compare = String.Map.compare String.compare
    let empty = String.Map.empty
  end

  let distributions =
    let package = "superset-disasm-reports" in
    let map_persistent =
      Knowledge.Persistent.of_binable (module SMap) in
    let attr ty persistent name desc =
      let open Theory.Program in
      Knowledge.Class.property ~package cls name ty
        ~persistent
        ~public:true
        ~desc in
    let open Knowledge.Domain in
    let smap_domain =
      mapping (module String) ~equal:(equal) "string_map"
    in
    attr smap_domain map_persistent "reports"
      "The reports for a given analysis"

end  

let collect_distributions superset threshold pmap =
  let open Bap_knowledge in
  let open Bap_core_theory in
  KB.promise Cache.distributions (fun o ->
      let open KB.Syntax in
      KB.collect Metrics.Cache.ground_truth o >>= fun tps ->
      let default = Cache.SMap.empty in
      KB.return @@ Option.value_map ~default tps ~f:(fun tps ->
          let init = String.Map.empty in
          let pmap =
            Map.mapi pmap ~f:(fun ~key ~data ->
                List.fold data ~init:String.Map.empty
                  ~f:(fun dist (p,addr,ftname) ->
                    String.Map.update dist ftname ~f:(fun total ->
                        (Option.value total ~default:0) + p
                      )
                  )
              ) in
          let reports =
            List.fold (Addr.Map.to_alist pmap) ~init ~f:(fun reports (p_at,fttot) ->
                String.Map.fold fttot ~init:reports
                  ~f:(fun ~key ~data reports ->
                    let name = key in
                    let p = data in
                    String.Map.update reports name ~f:(fun dist ->
                        let dist =
                          Option.value dist
                            ~default:(Distribution.empty threshold)
                        in Distribution.add dist tps (p,p_at,name)
                      )
                  )
              ) in
          reports
        )
    )
