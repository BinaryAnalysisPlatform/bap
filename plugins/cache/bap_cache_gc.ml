(**

   The main goal is to delete files randomly and prioritizing larger files,
   but still giving the probability for all files to be deleted.

   Notation:
   1. s(i) - the size of i-th file, where i = 0..m-1 with m being the
      total number of files;
   2. Sum(x(i)) = x(0) + ... x(m-1) - is the sigma operator;
   3. T = Sum(s(i)) - the total size of the cache;
   4. p(i) = s(i)/T - the discrete probability distrubution of the file
      sizes in cache, likelihood that a randomly chosen file from the
      cache will have size s(i).
   5. F(i) = p(i - 1) + p(i) cumulative discrete distribution function
      (CDF). F(i) we can generate a random number u in range 0..1,
      using a uniform random number generator, and then find such k that
      F(k-1) < u <= F(k).
   6. |s| = Sum(p(i) * s(i)) = (1/T) * Sum(s(i)^2) - the expected value
      of the size in cache
   7. |n| = t/|s| - the expected number of deletions that we need to
      make to delete t bytes, e.g. if we want to delete half:
      |n| = T^2 / (2*Sum(s(i)^2)

   Example:
    sizes = {4, 6, 3, 1, 6}
    the total size of the cache is Sum(sizes(i)) = 20
    the PDF is p(i) = {4/20; 6/20; 3/20; 1/20; 6/20}
    and CDF is F(i) = {4/20; 10/20; 13/20; 14/20; 20/20}

   We don't want to use floating points, there will be too many big and
   small numbers and overflows and we finally want to get an
   index. We will use rational numbers, since formulas 4. and 5. have the
   same denominator (namely T) we can use only numenators.

   On the high-level, we need to generate a random value between 0 and
   T, and find such k that F(k-1) < S <= F(k), the k-th file will be
   our candidate for removal. We can repeat sampling until we get |n|
   files (of course deleting the same file twice won't free twice of
   its size, so we had to keep in mind which files we already selected
   and repeat until we get |n| distinct files)
   Of course, we don't want to have a linear search for intervals, but
   we can see, that F(i) partitions the set of sizes (0...T) into m-1
   subsets, so we can represent F as a finite mapping, e.g., with our
   example,

   [0,3] -> 0
   [4,9] -> 1
   [10,12] -> 2
   [13,13] -> 3
   [14,19] -> 4

   Since intervals are not intersecting, we don't need to use
   Interval_map here, we just need to use the common Map from core
   with the closest_key (`Less_or_equal_to`` function. So once we
   generated a random size u we call for the closest_key for u and
   pick the associated value as the index of the file that we will
   delete. E.g., let's choose randomly a value from the range of
   0...19, if it in range from 0..3 we will pick the first file, or if
   it is in range from 4,9, e.g., 5, then closest_key will return 4,1,
   so we will remove the second file. So we managed to get away from
   ugly floats and got the desired distribution with no rounding
   errors.

   Now, after we have selected |n| distinct files we can shuffle them and
   delete without worrying that some other process already deleted one
   of those files. All the processes are using the same sequence of
   pseudorandom files, so they will select approximately equal files
   for deletion.

   And finally, we don't want to make our recursive selection depend
   from |n|, so instead of selecting |n| files for removal we will
   select as many files as we need to remove requested size,
*)

open Core_kernel
open Bap.Std

include Self ()

module Cfg = Bap_cache_config
module CDF = Int.Map

type entry = {
  path : string;
  size : int;    (* Kb  *)
}

let (//) = Filename.concat

let min_entry_size = 4 (* Kb *)

let entry path name =
  try
    let path = path // name in
    let size = Unix.( (stat path).st_size ) / 1024 in
    Some {path; size;}
  with _ -> None

(* TODO: think here *)
let read_cache path =
  Sys.readdir path |> Array.filter_map ~f:(entry path)

let total_size =
  Array.fold ~init:0 ~f:(fun s e -> s + e.size)

let cdf entries =
  let prob e = max min_entry_size e.size in
  fst @@
  Array.foldi entries ~init:(Map.empty (module Int),0)
    ~f:(fun i (m,prev) e ->
        let f_i = prev + prob e in
        CDF.add_exn m prev i, f_i)

let select entries total_size size_to_free =
  let size i = entries.(i).size in
  let rec loop cdf indexes freed =
    if freed < size_to_free then
      let u = Random.int total_size in
      let (_,i) =
        Option.value_exn (CDF.closest_key cdf `Less_than u) in
      if Set.mem indexes i then loop cdf indexes freed
      else loop cdf (Set.add indexes i) (freed + size i)
    else indexes in
  let cdf = cdf entries in
  loop cdf (Set.empty (module Int)) 0 |> Set.to_array

let remove e =
  try Sys.remove e.path
  with exn ->
    warning "unable to remove entry: %s" (Exn.to_string exn)

let shuffle fs =
  let gen = Random.State.make_self_init () in
  let len = Array.length fs in
  let rec loop n =
    if n < len then
      let () = Array.swap fs n (Random.State.int gen len) in
      loop (n + 1) in
  loop 0

let to_Kb s = Int64.(to_int_exn @@ s / 1024L)

let shrink ?threshold ~upto () =
  let entries = read_cache @@ Cfg.cache_data () in
  let total = total_size entries in
  let upper_bound = match threshold with
    | None -> to_Kb upto
    | Some t -> to_Kb t in
  if total > upper_bound then
    let selected = select entries total (total - to_Kb upto) in
    shuffle selected;
    Array.iter selected ~f:(fun i -> remove entries.(i))


let shrink ?threshold ~upto () =
  let t0 = Unix.gettimeofday () in
  shrink ?threshold ~upto ();
  let t1 = Unix.gettimeofday () in
  My_bench.update "gc" t0 t1

let clean () =
  Array.iter (read_cache @@ Cfg.cache_data ()) ~f:remove
