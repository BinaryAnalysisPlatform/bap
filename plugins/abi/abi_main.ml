let provides = ["abi"; "pass"]
let doc = {|
# DESCRIPTION

Runs ABI-specific passes. The passes are registered using
Bap_abi.register_pass function, by target-specific plugins,
depending on the binary architecture and command line options,
provided by a user.

# SEE ALSO

$(b,bap-plugin-x86)(1), $(b,bap-plugin-arm)(1), $(b,bap-abi)(3)
|}
open Bap.Std
open Bap_main


let () = Extension.declare ~provides ~doc (fun _ ->
    Project.register_pass ~autorun:true Bap_abi.pass;
    Ok ())
