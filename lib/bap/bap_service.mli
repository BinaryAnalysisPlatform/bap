open Core_kernel
open Bap_future.Std
open Regular.Std

type t
type service = t
type provider
type product

type inputs
type success
type failure

val declare : ?desc:string -> string -> service
val provide : ?desc:string -> service -> string -> product list -> provider
val require : product list -> inputs stream
val no_deps : product list
val providers : service -> provider list
val optional : service -> product
val required : service -> product
val parameter : 'a Bap_self.param -> product
val binary : product
val undefined : product
val inputs : provider -> inputs stream
val digest : inputs -> digest
val get : inputs -> 'a Bap_self.param -> 'a
val success_or_die : (success, failure) result -> success
val die_on_failure : (success, failure) result -> unit
val run :
  ?providers:provider list ->
  ?options:(string * string) list ->
  ?argv:string array ->
  ?input:[`Data of Bigstring.t | `Path of string] ->
  unit -> (success, failure) result

(**
   {[
     let salad = Service.(provide fruit_salad "G&K Fruit Salads" [
         required oranges;
         optional bananas;
         optional strawberries;
         one_from [salt; sugar];
         parameter ignore_client_requests;
         parameter double_pricing;
         parameter respect_customers;
         parameter use_rotten_food;
       ])
   ]}
*)

(**

   A service is just a name, that could be used to define dependencies
   between components. Instead of depending on a particular compoment,
   we depend on a component abstraction.

   Since kinds of provided services are very different we do not
   specify any interface. So a service is opaque.

   A service provider is required to provide a service that it claims
   to provide. It is also required that a service provider will act as
   a pure function of its inputs.

   The inputs of a service provider are a set of parameters, input
   binaries (which we can also represent as a parameter, btw), and the
   provider code itself, which is extracted from the digest of a
   bundle that provides it (so a change in the code of a provider,
   will be reflected in the change of its inputs). A provider must
   provide the same output for the same inputs.

   The provider declares its inputs using the [provide] function that
   accepts a list of inputs. Once the inputs are obtained the provider
   is called (either via the new provider inteface, or via the old
   [when_ready] interface) and is passed with the input specification
   as well as a special token value which could be used to obtain the
   current value of parameters. So far, it is the provider
   responsibility to enable caching (which should be easy as the
   digest is already provided), but the framework may decide not
   to call the provider at all if the downstream consumers are already
   satisfied (aka backward chaining), however as of today we propagate
   changes forward.





*)
