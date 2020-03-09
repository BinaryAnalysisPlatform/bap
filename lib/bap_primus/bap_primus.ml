module Std = struct
  module Primus = struct
    open Bap_primus_generator_types
    include Bap_primus_types
    module Iterator = Iterator
    module Env = Bap_primus_env
    module Generator = Bap_primus_generator
    module Interpreter = Bap_primus_interpreter
    module Time = Interpreter.Time
    module Linker = Bap_primus_linker
    module Value = Bap_primus_value
    module Memory = Bap_primus_memory
    module Observation = Bap_primus_observation
    module Lisp = Bap_primus_lisp
    module Analysis = Bap_primus_analysis.Machine
    module System = Bap_primus_system
    module Job = System.Job
    module Jobs = System.Jobs
    module Info = Bap_primus_info
    module Components = System.Components
    module Machine = struct
      module type State = State
      include Bap_primus_machine
      type 'a state = 'a State.t
      include Bap_primus_main
      let finished = System.fini
      let init = System.init
    end
    type generator = Generator.t
    let sexp_of_value = Value.sexp_of_t
    let value_of_sexp = Value.t_of_sexp
    let compare_value = Value.compare
    type system = System.t
    type info = Info.t
  end
end
