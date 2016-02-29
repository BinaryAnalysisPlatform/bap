open Ocamlbuild_plugin
open Bap_build.Std


let main () =
  Plugin_options.set ();
  Ocamlbuild_plugin.dispatch (function
      | Before_rules -> Plugin_rules.install ()
      | _ -> ());
  Ocamlbuild_unix_plugin.setup ();
  Ocamlbuild_pack.Main.main ()

let () = main ()
