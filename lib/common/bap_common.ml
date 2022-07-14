
module Plugins = Bap_common_private_sites.Plugins.Plugins

open Bap_common_private_sites.Sites
let sites = function
  | "api" -> api
  | "lisp" -> lisp
  | "plugins" -> plugins
  | "primus" -> primus
  | "semantics" -> semantics
  | "signatures" -> signatures
  | "site-lisp" | "site_lisp" -> site_lisp
  | unknown ->
    failwith ("Invalid site: " ^ unknown)
