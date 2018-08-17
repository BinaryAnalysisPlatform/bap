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
val access : inputs -> 'a Bap_self.param -> 'a
val success_or_die : (success, failure) result -> success
val die_on_failure : (success, failure) result -> unit
val run : provider list -> (success, failure) result

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
