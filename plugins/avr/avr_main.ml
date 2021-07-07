open Bap_main

let main _ctxt =
  Bap_avr_target.load ();
  Ok ()


let () = Bap_main.Extension.declare main
    ~provides:[
      "avr";
      "lifter";
    ]
