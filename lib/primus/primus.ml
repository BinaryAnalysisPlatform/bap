module Std = struct
  module Primus = struct
    include Primus_types
    module Env = Primus_env
    module Error = Primus_error
    module Generator = Primus_generator
    module Interpreter = Primus_interpreter
    module Linker = Primus_linker
    module Machine = Primus_machine
    module Memory = Primus_memory
    module Scheduler = Primus_scheduler
    module Observation = Primus_observation
    module Lisp = Primus_lisp
  end
end
