let () =
  Arg.write_arg "config.status.in" @@
  Array.sub Sys.argv 1 (Array.length Sys.argv - 1)
