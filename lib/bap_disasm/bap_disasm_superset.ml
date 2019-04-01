type 'a t = 'a Superset.t
let raw_superset = Superset.superset_disasm_of_file
let trimmed_superset = Trim.trimmed_disasm_of_file
let converged_superset s = Trim.Default.trim Features.(apply_featurepmap Features.default_features s)
