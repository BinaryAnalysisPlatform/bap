module Primus = struct
  include Bap_primus_types
  module Env = Bap_primus_env
  module Generator = Bap_primus_generator
  module Linker = Bap_primus_linker
  module Machine = struct
    module type State = State
    include Bap_primus_machine
    type 'a state = 'a State.t
  end
  module Value = Bap_primus_value
  module Memory = Bap_primus_memory
  module Observation = Bap_primus_observation
  type generator = Generator.t
  let sexp_of_value = Value.sexp_of_t
  let value_of_sexp = Value.t_of_sexp
  let compare_value = Value.compare
  type 'a machine = 'a Machine.t
  type component = Machine.Component.t
end
