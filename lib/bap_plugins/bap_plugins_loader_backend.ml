let load = ref Dynlink.loadfile

let install loader = load := loader
let load x = !load x
