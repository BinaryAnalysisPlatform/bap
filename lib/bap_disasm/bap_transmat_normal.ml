open Bap.Std
open Core_kernel.Std
open Bigarray
open Bap_insn_aliasing

type mat = (float, float64_elt, c_layout) Array2.t
type bijective_insnmap = int String.Table.t * string Int.Table.t
type corpus = insn memmap
type t = {tmat : mat; arch : Arch.t; insn_ids : bijective_insnmap;
          size : int}

let column_total mat i j =
  let rec total j =
    if not (j = 0) then (mat.{i,j} +. (total (j-1))) else mat.{i,j}
  in total j

let probabilities newmat oldmat size = 
  let rec initialize i j =
    if not (i > 0) then
      newmat.{i,j} <- oldmat.{i,j} /. (column_total oldmat i (size-1));
    initialize (i-1) j in
  initialize (size-1) (size-1)

let copy newmat oldmat size =
  let rec initialize i j =
    if (not (i > 0)) then (
      newmat.{i, j} <- oldmat.{i,j};
      initialize i (j-1)
    ) else
      initialize (i-1) j in initialize size size

(* TODO test to make sure that order of i, j is correct *)
let transition_of_data totals =
  let size = Array2.dim1 totals in
  let transitions = Array2.create Float64 c_layout size size in
  probabilities transitions totals size

let totals_fname arch = (Arch.to_string arch) ^ "_transition_totals"
let insn_map_fname arch = (Arch.to_string arch) ^ "_insn_ids"

let totals_of_arch arch =
  let ids_to_insns = (Int.Table.create ()) in
  let insns_file = (insn_map_fname arch) in
  let insns_to_ids = (if (Sys.file_exists insns_file) then (
      let contents = In_channel.with_file
          insns_file ~f:In_channel.input_all in
      (Sexp.of_string contents |> String.Table.t_of_sexp
         (Int.t_of_sexp));
    ) else (String.Table.create ())) in
  Hashtbl.iter insns_to_ids ~f:(fun ~key ~data ->
      ignore (Hashtbl.add ids_to_insns ~key:data ~data:key));
  let s = (totals_fname arch) in
  let mat, size = if (Sys.file_exists s) then (
    let contents = In_channel.with_file s ~f:In_channel.input_all in
    let elements = Sexp.of_string contents |>
                   List.t_of_sexp
                     (fun srow -> (
                          List.t_of_sexp
                            (fun element ->
                              Float.t_of_sexp element) srow))  in
    let size = max 500 (List.length elements) in
    let mat = Array2.create Float64 c_layout size size in
    Array2.fill mat 0.0;
    let total = ref 0.0 in
    List.iteri elements ~f:(fun i row ->
        List.iteri row ~f:(fun j column_elem ->
            total := !total +. column_elem;
            (mat.{i,j} <- column_elem)
          )
      );
    print_endline ("total: " ^ Float.to_string !total);
    mat, size
  ) else Array2.create Float64 c_layout 500 500, 500 in
  { tmat=mat; arch; insn_ids=(insns_to_ids, ids_to_insns); size }

let of_arch arch =
  let { tmat=totals; arch; insn_ids; size } = totals_of_arch arch in
  let insn_to_ids, ids_to_insns = insn_ids in
  { tmat = (transition_of_data totals); arch;
    insn_ids = (insn_to_ids, ids_to_insns); size }

let write_transmat transmat =
  let convert transmat =
    let size = (Array2.dim1 transmat.tmat) -1 in
    let rec fill1 i j = 
      let rec fill2 j =
        if j > 0 then
          (transmat.tmat.{i,j}) :: (fill2 (j-1))
        else [transmat.tmat.{i,j}] in
      if i > 0 then
        (fill2 j) :: fill1 (i-1) j
      else [fill2 j] in fill1 size size in
  let data = convert transmat in      
  let data = List.sexp_of_t
      (fun column -> List.sexp_of_t Float.sexp_of_t column)
      data |> Sexp.to_string in
  Out_channel.write_all (totals_fname transmat.arch) data;
  let insn_to_ids,_ = transmat.insn_ids in
  let ids = String.Table.sexp_of_t Int.sexp_of_t insn_to_ids in
  Out_channel.write_all
    (insn_map_fname transmat.arch) (Sexp.to_string ids)

let remap_all transmat =
  let size = Array2.dim1 transmat.tmat in
  let to_replace =
    Array2.create Float64 c_layout size size in
  Array2.fill to_replace 0.0;
  copy to_replace transmat.tmat transmat.size


let map_insn transmat insn =
  let instructions = transmat.insn_ids in
  let count = Hashtbl.length @@ fst instructions in
  (match (Hashtbl.find (fst instructions) (Insn.name insn)) with
  | None -> (
      (match (Hashtbl.add
                (fst instructions)
             ) ~key:(Insn.name insn) ~data:(count) with
      | _ -> ());
      (match (Hashtbl.add
                (snd instructions) ~key:(count) ~data:(Insn.name insn)) with
      | _ -> ());
    )
  | _ -> ());
  if (count >= transmat.size) then
    remap_all transmat, count+1
  else transmat, count+1

let update_from_shingles transmat corp =
  let zero = (Addr.of_int ~width:(Size.to_bits @@ Arch.addr_size transmat.arch) 0) in
  let min_addr = Option.value (Memmap.min_addr corp) ~default:zero in
  let max_addr = Option.value (Memmap.max_addr corp) ~default:zero in
  let targets = get_targets min_addr max_addr in
  let final = Memmap.to_sequence corp |> Seq.fold ~init:transmat ~f:(fun accu (mem, oinsn) ->
      List.fold ~init:accu (targets mem oinsn)
          ~f:(fun accu (addr, etype) ->
              match addr with
              | Some addr -> 
                Seq.fold ~init:accu (Memmap.lookup corp addr)
                  ~f:(fun transmat (targ_mem, targ_insn) ->
                      if (addr=(Memory.min_addr targ_mem)) then (
                        let accu, target =
                          map_insn accu targ_insn in
                        let accu, origin = 
                          map_insn accu oinsn in
                        (accu.tmat.{target, origin }
                         <- accu.tmat.{target, origin } +. 1.0); 
                        accu
                      ) else transmat)
          | None -> transmat (* cannot associate a statistic with a target
                          that may not have been computable  *)
        );
    ) in
  write_transmat final

let update_from_disasm transmat disasm = 
  let corp = ref Memmap.empty in
  Seq.iter (Disasm.insns disasm)
    ~f:(fun (mem, insn) -> (corp := Memmap.add !corp mem insn));
  update_from_shingles transmat !corp

let transition transmat from_insn to_insn =
  let transmat, target = map_insn transmat to_insn in
  let transmat, origin = map_insn transmat from_insn in
  (transmat.tmat).{target, origin}

