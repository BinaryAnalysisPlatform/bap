(** bapbuild implementation library *)


(** bapbuild support library.

    Underneath the hood [bapbuild] is just a plugin to
    [Ocamlbuild]. Instead of using the [bapbuild] utility, it is
    possible to use custom [myocamlbuild.ml]. In that case to activate
    the bapbuild behavior it is needed to call [Plugin_options.set ()]
    function, and and install plugin rules in the [Before_rules]
    phases, e.g.,

    {[
      open Bap_build.Std

      let () =
        Plugin_options.set ();
        Ocamlbuild_plugin.dispatch (function
            | Before_rules -> Plugin_rules.install ()
            | _ -> ());

    ]}


*)
module Std : sig
  module Plugin_rules : sig

    (** [install ()] installs bap specific rules.

        The function installs rules necessary for building bap
        plugins.*)
    val install : unit -> unit
  end

  module Plugin_options : sig


    (** [set ()] set default options for [bapbuild].  *)
    val set : unit -> unit
  end
end
