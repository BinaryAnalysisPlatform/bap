open Bap_core_theory
open Base
open KB.Syntax
open A64_exceptions
open A64_defs

module Env  = A64_env.Env


module Bits(Core : Theory.Core) = struct
	open Core

	module Utils = A64_utils.Utils(Core)
  open Utils


end