open Bap_knowledge

module Sort = Bap_core_theory_sort.Sort
include Semantics.Sorted(Sort)
let sort = kind
