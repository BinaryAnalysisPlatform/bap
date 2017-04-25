(** Microexecution library.

    Microexecution is an execution of arbitrary pieces of code,
    under random context.

    The library provides two classes:

    - a concretizer
      concretization is a process of giving a value for unknown
      variable);
    - a conqueror, that is an IR interpreter, that tries to cover as
      much code as needed.

*)

[@@@deprecated "use Primus instead"]

module Std = struct
  module Concretizer = Microx_concretizer
  module Conqueror   = Microx_conqueror

  class ['a] concretizer = ['a] Concretizer.main
  class ['a] conqueror   = ['a] Conqueror.main
end
