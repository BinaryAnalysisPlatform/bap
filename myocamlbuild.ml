(* OASIS_START *)
(* OASIS_STOP *)
let oasis_env =
  BaseEnvLight.load
    ~filename:MyOCamlbuildBase.env_filename
    ~allow_empty:true
    ()
let nonempty = function (A s) -> String.length s != 0 | _ -> true
let expand s = BaseEnvLight.var_expand s oasis_env;;

(* the piqi support is rather fragile *)
rule "piqic: piqi -> .ml & _ext.ml"
  ~prods:["%_piqi.ml"; "%_piqi_ext.ml"]
  ~deps:["%.piqi"]
  (fun env _ ->
     Cmd (S (List.filter nonempty [
         A (expand "${piqic}");
         A (expand "${piqic_flags}");
         A "--debug";
         A "1";
         A "-C";
         A "lib/bap_types";
         A "-I";
         A "../lib/bap_types";
         A (env "%.piqi");
         A"--multi-format"])));;


let dispatch = function
  | After_rules ->
    List.iter
      (fun tag ->
         pflag ["ocaml"; tag] "pa_ounit_lib"
           (fun s -> S[A"-ppopt"; A"-pa-ounit-lib"; A"-ppopt"; A s]))
      ["ocamldep"; "compile"; "doc"];
  | _ -> ()

let () = Ocamlbuild_plugin.dispatch (fun hook ->
    dispatch hook; dispatch_default hook)
