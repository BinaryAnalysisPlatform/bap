(** ABI dispatcher.

    This library accompanies the abi pass, and allows to inject
    arbitrary transformation on the abi recognition phase. The
    abi pass is run before the api pass.


    The library and the pass have no specific functionality, other
    than running the passes, that were registered by specific
    compiler, language or architecture specific modules.
*)

open Bap.Std

(** [pass] will apply all registered passes in the unspecified order  *)
val pass : project -> project

(** [register_pass pass] registers a pass for the later execution.
    The pass will be run by a [api] pass. Usually the [pass] will
    inspect the project structure, and if it is not recognized, then
    it will just return the project untouched, otherwise it may
    apply some transformations on the project, e.g., demangling, and
    register further actions in the system, e.g., api transformations.*)
val register_pass : (project -> project) -> unit


val name : string tag
