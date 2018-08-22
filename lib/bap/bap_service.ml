open Core_kernel
open Bap_future.Std
open Regular.Std
open Future.Syntax
open Bap_bundle.Std

type t = Service of string
type service = t
type success = Success
type failure =
  | Unsat of {
      kind : string;
      name : string;
      problem : string;
    }

  | Spec of Bap_self.error

type outcome = (success, failure) result
type 'a maybe = Defined of 'a | Undefined
type inputs = {digest : digest maybe}
type product = {inputs : inputs stream}
type provider = {
  name : string;
  service : service;
  product : product;
}

(* unique service and provider identifiers together with their descriptions *)
let registry = String.Table.create ()

(* the allotment of all providers *)
let providers : provider list ref = ref []


(* is signaled at the beginning of each distribution phase *)
let starts,start = Stream.create ()

(* is signal once the distribution is finished  *)
let finishes,finish = Stream.create ()

(* no declarations shall happen after *)
let declared = Future.ignore_m (Stream.hd starts)

let fails,fail = Stream.create ()

let run ?providers:selection ?options ?argv ?input () =
  let selection = match selection with
    | None -> !providers
    | Some selection -> selection in
  let failed = Stream.hd fails in
  Signal.send start selection;
  Signal.send finish ();
  match Future.peek failed with
  | Some failure -> Error failure
  | None -> match Bap_self.run ?options ?argv ?input () with
    | Ok _ -> Ok Success
    | Error err -> Error (Spec err)



let inputs provider = provider.product.inputs
let digest =
  let state = Random.State.make_self_init () in
  let generate () =
    Data.Cache.digest ~namespace:"undefined" "%s" @@
    String.concat_array @@
    Array.init 16 ~f:(fun _ ->
        sprintf "%x" (Random.State.int state 256)) in
  fun inputs -> match inputs.digest with
    | Defined digest -> digest
    | Undefined -> generate ()


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
  if Future.is_decided declared
  then invalid_argf
      "The declaration of a %s named %s failed, because it occurs
      after service scheduling has started. All declarations shold
      be made on module intialization level. " kind name ()

let declare ?desc name =
  validate_stage "service" name;
  register_identifier ?desc "service" name;
  Service name

let all_inputs products =
  Stream.all @@
  List.map products ~f:(fun p -> p.inputs)


let concat_inputs x y = match x.digest, y.digest with
  | Defined x, Defined y ->
    {digest = Defined (Data.Cache.Digest.concat x y)}
  | _ -> {digest=Undefined}

let provide ?desc (Service s) name products : provider =
  let namespace = "provider for " ^ s in
  validate_stage namespace name;
  register_identifier ?desc namespace name;
  let init = {digest=Defined (Data.Cache.Digest.create ~namespace)} in
  let inputs = Stream.map (all_inputs products) ~f:(fun inputs ->
      List.fold inputs ~init ~f:concat_inputs) in
  {name; service = Service s; product = {inputs}}

let require =
  let next_number = ref 0 in
  fun products ->
    incr next_number;
    let bundle = main_bundle () in
    let manifest = Bundle.manifest bundle in
    let name = sprintf "edu.cmu.ece.bap.anonymous/%s/%d"
        (Manifest.name manifest)
        !next_number in
    let service = declare name in
    let provider = provide service name products in
    provider.product.inputs


let providers_of_service  service providers =
  List.filter providers ~f:(fun p -> p.service = service)

let providers service = providers_of_service service !providers



let inputs provider = provider.product.inputs

let products service =
  declared >>| fun () ->
  List.map (providers service) ~f:(fun p -> p.product)

let no_deps = []


let fail kind name problem = Signal.send fail (Unsat {
    kind; name; problem
  })

(* list of selected products per each selection *)
let selected_products service =
  Stream.map starts ~f:(providers_of_service service) |>
  Stream.map ~f:(List.map ~f:inputs)

(* a stream of inputs provided for each service, linearized *)
let selected_inputs service =
  Stream.join @@
  Stream.map (selected_products service) ~f:Stream.concat


let product ~required (Service s as service) = {
  inputs =
    selected_inputs service |>
    Stream.frame ~clk:finishes ~init:[] ~f:(fun xs x -> x :: xs) |>
    Stream.filter_map ~f:(fun xs ->
        match List.reduce ~f:concat_inputs xs with
        | None ->
          if required
          then fail "service" s "is required but not provided";
          None
        | ok -> ok)
}

let optional = product ~required:false
let required = product ~required:true

let file_contents ready path () =
  let md5sum = Digest.file path in
  let content = Data.Cache.digest ~namespace:"file" "%s" md5sum in
  Signal.send ready {digest = Defined content}

let content path = {
  inputs = Stream.filter_map starts ~f:(fun _ ->
      try
        let md5sum = Digest.file path in
        let content = Data.Cache.digest ~namespace:"file" "%s" md5sum in
        Some {digest = Defined content}
      with Sys_error msg ->
        fail "file" path ("is unaccessible - " ^ msg);
        None)
}

let undefined : product = {
  inputs = Stream.map starts ~f:(fun _ -> {digest = Undefined})
}

(* inputs witnesses that run was run, due to the lack
   of linearity in the type system we can't prevent users from
   reusing the same witness multiple times, however in the future
   we may equip [inputs] with a unique run id and check that correct
   inputs are applied to correct versions of a
*)
let get _ p = Bap_self.Param.current p

(* it should be Info.file, but let's wait while we determine module dependencies *)
let binary = undefined

(* that's a stub...*)
let parameter p =
  let inputs = Bap_self.Param.values p |>
               Stream.map ~f:(fun x ->
                   {digest = Defined (Bap_self.Param.digest p x)}) in
  {inputs}

let arch = undefined

let pp_failure ppf = function
  | Unsat {kind; name; problem} ->
    Format.fprintf ppf
      "Failed to satisfy all service requests. The %s %s %s."
      kind name problem
  | Spec _error ->
    Format.fprintf ppf
      "Failed to parse parameter specification... TBD"

let success_or_die = function
  | Ok Success -> Success
  | Error fail ->
    Format.eprintf "%a@\n%!" pp_failure fail;
    exit 1

let die_on_failure result = ignore (success_or_die result)
