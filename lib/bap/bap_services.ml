let loader = Bap_service.declare
    "edu.cmu.ece.bap.std/loader"
    ~desc:"binary format parser with the OGRE output"

let backend = Bap_service.declare
    "edu.cmu.ece.bap.legacy/backend"
    ~desc:"binary format parser that generates images"


let brancher = Bap_service.declare
    "edu.cmu.ece.bap.std/brancher"
    ~desc:"computes potential destinations for branches"

let rooter = Bap_service.declare
    "edu.cmu.ece.bap.std/rooter"
    ~desc:"searches for function starts in binary code"

let lifter = Bap_service.declare
    "edu.cmu.ece.bap.std/lifter"
    ~desc:"provides instruction semantics"

let reconstructor = Bap_service.declare
    "edu.cmu.ece.bap.std/reconstructor"
    ~desc:"reconstructs function boundaries"

let symbolizer = Bap_service.declare
    "edu.cmu.ece.bap.std/symbolizer"
    ~desc:"provides symbolic names to program locations"

let disassembler = Bap_service.declare
    "edu.cmu.ece.bap.std/disassembler"
    ~desc:"decodes bytes into assembly instructions"


let abi = Bap_service.declare
    "edu.cmu.ece.bap.std/abi"
    ~desc:"provides ABI specific information"
