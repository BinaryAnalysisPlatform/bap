
(** Bap-service library.

    {1 Overview}

    The library establishes simple relationships between different
    sides of computation.

    The [service] is an abstraction of resources on which a
    computation may depend.

    Services are provided by service [providers]. The same service
    could be provided by several providers. Only a service provider
    knows how the provided service depends on the environment.

    A [product] is a result of a particular comptutation, that depends
    of computational environment. *)

open Core_kernel
open Bap_future.Std
open Regular.Std


type service
type provider
type product

type inputs
type success
type failure
type modality

(** [declare ?desc name] declares a new service with the specified
    name and description.

    The service name should be globally unique and the program will
    terminate if it is not the case. The service [name] should be a
    valid identifier, which should be a non-empty sequence of
    alphanumeric characters, '/', '_', '.', or '-'.

    The main purpose of the service declaration is to declare the name
    of the service and make it available to other possible providers
    and consumers.
*)
val declare : ?desc:string -> string -> service


(** [provide ?desc service name requirements] declares a provider
    of the specified service, that in turn may have its own
    requirements. The provider name should be unique, and an
    optional description might be provided to help the users make
    the right selection of a provider for the given service.

    The provider name shall be a valid identifier that follows the
    same rules as the service name, see [declare] for more
    information. The identifier should be unique in the namespace of
    the service that it is providing. I.e., its fine to reuse the same
    name of a provider for different services.
*)
val provide : ?desc:string -> service -> string -> require:product list -> provider


(** [require requirements] establishes [requirements] without
    specifying a service or a provider.

    It is common to write an analysis that although provides some
    service (otherwise why to write it at all), this service is
    intented to be used by only one person, namely the author of
    analysis. However, the analysis might still require some other
    services thus the Service facility woudld be necessary. Thus
    In that case, instead of creating a dummy service along
    with a dummy provider, a user can just specify the set of
    requirements.

    Note: this function will create a unique service and provider pair,
    that might be observable in case if the service facility would
    unable to satisfy the requirements and terminate the program with
    an error. The function will do its best to provide a recognizable
    an readable name (by leveraging the plugin subsystem), but may
    fallback just to a random identifier.

    Since an anonymous provider could not be selected or deselected
    the [require] function returns directly the inputs future, that
    will be determined to the inputs of the analysis while they are
    ready.
*)
val require : product list -> inputs future

(** [nothing  []] an empty list that is provided here for
    readability, e.g.,
    [require nothing] or
    [provide grooming "com.ivg.groomer" ~require:nothing]
*)
val nothing : product list
val product : modality -> service -> product
val program : string -> product
val content : string -> product
val library : string -> product
val env_var : string -> product
val cmdline : string -> product


val required : modality
val optional : modality


val providers : service -> provider list

val inputs : provider -> inputs future
val digest : inputs -> digest

val success_or_die : (success, failure) result -> success
val die_on_failure : (success, failure) result -> unit

(** Distribute products.

    Example:
    {[
      die_on_failure @@ run ()
    ]}

*)
val run : provider list -> (success, failure) result

(**
   {[
     let salad = Service.(provide fruit_salad "G&K Fruit Salads" [
         product required oranges;
         product optional bananas;
         product optional strawberries;
         product one_from [salt; sugar];
         library "libdressings";
         content "/etc/version";
         program "mixer";
         env_var "IGNORE_CLIENT_REQUESTS";
         cmdline "double-pricing";
         cmdline "respect-customers";
         cmdline "use-rotten-food";
       ])
   ]}
*)


(* files, environment variables, are all services *)
