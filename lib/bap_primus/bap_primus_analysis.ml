open Bap.Std
open Bap_knowledge

module Machine = struct
  type 'a m = 'a Knowledge.t
  include Bap_primus_machine.Make(Knowledge)
  let collect p o = lift (Knowledge.collect p o)
  let resolve p o = lift (Knowledge.resolve p o)
  let provide p o x = lift (Knowledge.provide p o x)
  let suggest a p o x = lift (Knowledge.suggest a p o x)
end
