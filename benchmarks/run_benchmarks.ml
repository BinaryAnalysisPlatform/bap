open Core.Std
open Core_bench.Std
open Bap.Std

let benchmarks = Bench.make_command [
    Bench_image.test;
  ]

let () = Command.run benchmarks
