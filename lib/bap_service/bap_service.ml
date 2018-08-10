open Core_kernel
open Bap_future.Std
open Regular.Std
open Future.Syntax

type service = Service of string
type success = Success
type failure = Unsat of {
    kind : string;
    name : string;
    problem : string;
  }

type outcome = (success, failure) result
(* we might later extend it to provide more introspection *)
type inputs = {digest : digest}

type product = {inputs : inputs future}

type provider = {
  name : string;
  service : service;
  product : product;
}

type 'a requirement = ('a -> product) -> 'a -> product

(* unique service and provider identifiers together with their descriptions *)
let registry = String.Table.create ()

(* the allotment of all providers *)
let providers : provider list ref = ref []

(* happens when the run cycle is started *)
let started,start = Future.create ()

(* this will happen when the run cycle is finished,
   and may have the failure outcome.
*)
let (failed : failure future),
    (fail : failure promise) =
  Future.create ()


let run selection =
  providers := selection;
  Promise.fulfill start ();
  match Future.peek failed with
  | None -> Ok Success
  | Some failure -> Error failure

let inputs provider = provider.product.inputs
let digest inputs = inputs.digest

let no_desc = "no description provided"

let validate_identifier what name =
  let is_valid = function
    | '-' | '/' | '_' | '.' -> true
    | c -> Char.is_alphanum c in
  if String.is_empty name
  then invalid_argf
      "Can't declare a %s with an empty name. Please, provide a \
       valid identifier" what ();
  match String.find name ~f:(Fn.non is_valid) with
  | None -> ()
  | Some invalid ->
    invalid_argf
      "Can't declare a %s with name '%s'. Character '%c' is not \
       allowed in the service identifier" what name invalid ()

let register_identifier ?(desc=no_desc) kind name =
  validate_identifier kind name;
  let id = kind ^ name in
  if Hashtbl.mem registry id
  then invalid_argf
      "Failed to declare a new %s named %s as there already exists \
       one with the same name. Please choose a different identifier."
      kind name ();
  Hashtbl.set registry ~key:id ~data:desc

let validate_stage kind name =
  if Future.is_decided started
  then invalid_argf
      "The declaration of a %s named %s failed, because it occurs
      after service scheduling has started. All declarations shold
      be made on module intialization level. " kind name ()

let declare ?desc name =
  validate_stage "service" name;
  register_identifier ?desc "service" name;
  Service name

let digests products =
  Future.List.all @@
  List.map products ~f:(fun {inputs} -> inputs >>| fun {digest} -> digest)


let provide ?desc (Service s) name ~require:products : provider =
  let namespace = "provider for " ^ s in
  validate_stage namespace name;
  register_identifier ?desc namespace name;
  let digest = digests products >>| fun digests ->
    let init = Data.Cache.Digest.create ~namespace in
    List.fold digests ~init ~f:Data.Cache.Digest.concat in
  let inputs = digest >>| fun digest -> {digest} in
  {name; service = Service s; product = {inputs}}


let require =
  let next_number = ref 0 in
  fun products ->
    incr next_number;
    let name = sprintf "anonymous/%d" !next_number in
    let service = declare name in
    let provider = provide service name ~require:products in
    provider.product.inputs

let providers service =
  List.filter !providers ~f:(fun p -> p.service = service)

let inputs provider = provider.product.inputs

let products service =
  started >>= fun () ->
  digests @@  List.map (providers service) ~f:(fun p -> p.product)

let nothing = []

type modality = Required | Optional

let required = Required
let optional = Optional

let fail kind name problem = Promise.fulfill fail (Unsat {
    kind; name; problem
  })

let product modality (Service s as service) : product =
  let ready, contract = Future.create () in
  begin Future.upon (products service) @@ function
    | [] -> if modality = Required then
        fail "service" s "is required but not provided"
    | xs ->
      Promise.fulfill contract @@
      {digest=List.reduce_exn xs ~f:Data.Cache.Digest.concat}
  end;
  {inputs=ready}


let file_contents contract path () =
  let md5sum = Digest.file path in
  let digest = Data.Cache.digest ~namespace:"file" "%s" md5sum in
  Promise.fulfill contract {digest}

let content path =
  let ready, contract = Future.create () in
  begin try
      Future.upon started (file_contents contract path)
    with Sys_error msg ->
      fail "file" path ("is unaccessible - " ^ msg)
  end;
  {inputs=ready}

let program_in_path program =
  let ready, contract = Future.create () in
  begin try Future.upon started @@
      file_contents contract (FileUtil.which program)
    with _ ->
      fail "program" program ("is not available")
  end;
  {inputs=ready}

let program name =
  if Filename.is_implicit name
  then program_in_path name
  else content name


(* that's a stub...*)
let library _ = program Sys.argv.(0)
let cmdline _ = program Sys.argv.(0)
let env_var _ = program Sys.argv.(0)

let pp_failure ppf (Unsat {kind; name; problem}) =
  Format.fprintf ppf
    "Failed to satisfy all service requests. The %s %s %s."
    kind name problem

let success_or_die = function
  | Ok Success -> Success
  | Error fail ->
    Format.eprintf "%a@\n%!" pp_failure fail;
    exit 1

let die_on_failure result = ignore (success_or_die result)
