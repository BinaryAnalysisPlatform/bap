open Core
open Core_bench.Std
open Bap.Std

let benchmarks = Bench.make_command @@ List.concat [
    Bench_dom.tests;
    Bench_image.tests;
  ]


let () = Command.run benchmarks
