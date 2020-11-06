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
   5. F(i) = p(i) + p(i-1) + ... + p(0)
      cumulative discrete distribution function (CDF).
      F(i) we can generate a random number u in range 0..1,
      using a uniform random number generator, and then find such k that
      F(k-1) < u <= F(k).
   6. |s| = Sum(p(i) * s(i)) = (1/T) * Sum(s(i)^2) - the expected value
      of the size of a cache entry
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
   select as many files as we need to remove requested size.
*)

open Core_kernel
open Bap.Std

include Self ()

module Cache = Bap_cache
module CDF = Int.Map
module Unix = Caml_unix

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

let read_cache path =
  Sys.readdir path |> Array.filter_map ~f:(entry path)

let total_size =
  Array.fold ~init:0 ~f:(fun s e -> s + e.size)

let cdf entries =
  fst @@
  Array.foldi entries ~init:(Map.empty (module Int),0)
    ~f:(fun i (m,prev) e ->
        let f_i = prev + max min_entry_size e.size in
        CDF.add_exn m prev i, f_i)

let select entries total_size size_to_free =
  let cdf = cdf entries in
  let rec loop indexes freed =
    if freed < size_to_free then
      let u = Random.int total_size in
      let (_,i) =
        Option.value_exn (CDF.closest_key cdf `Less_than u) in
      if Set.mem indexes i
      then loop indexes freed
      else loop (Set.add indexes i) (freed + entries.(i).size)
    else indexes in
  loop (Set.empty (module Int)) 0 |> Set.to_array

let remove e =
  try Sys.remove e.path
  with exn ->
    warning "unable to remove entry: %s" (Exn.to_string exn)

let shuffle fs =
  Array.permute ~random_state:(Random.State.make_self_init ()) fs

let to_Kb s = s * 1024

let lower_bound c =
  let open Bap_cache_types in
  to_Kb @@
  max 0
    (c.capacity - (c.capacity * c.overhead / 100))

let shrink ?(by_threshold=false) cfg =
  let entries = read_cache @@ Cache.data () in
  let total = total_size entries in
  let lower_bound = lower_bound cfg in
  let max_size =
    if by_threshold then to_Kb @@ Cache.gc_threshold cfg
    else lower_bound in
  if total > max_size then
    let selected = select entries total (total - lower_bound) in
    shuffle selected;
    Array.iter selected ~f:(fun i -> remove entries.(i))

let clean () =
  Array.iter (read_cache @@ Cache.data ()) ~f:remove
