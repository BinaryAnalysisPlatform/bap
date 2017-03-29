module Std = struct
  module Primus = struct
    open Bap_primus_generator_types
    include Bap_primus_types
    module Iterator = Iterator
    module Env = Bap_primus_env
    module Error = Bap_primus_error
    module Generator = Bap_primus_generator
    module Interpreter = Bap_primus_interpreter
    module Linker = Bap_primus_linker
    module Machine = struct
      module type State = State
      include Bap_primus_machine
      type 'a state = ('a,Context.t) State.t
    end
    module Memory = Bap_primus_memory
    module Scheduler = Bap_primus_scheduler
    module Observation = Bap_primus_observation
    module Lisp = Bap_primus_lisp
    type generator = Generator.t
  end
end
