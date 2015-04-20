open Bap.Std

(** [extract_symbols output_path] will emit a script, that will
    extract symbols from IDA database and store it [output_path] *)
val extract_symbols : string -> string


(** for each (tag,script) pair attached to memory region [mem],
    if [tag] is equal to [idapy], then the [script] is added to the
    output program. The following substitution are avaliable in the
    script:

    $min_addr  - starting address of the [mem]
    $max_addr  - the addres of the last byte of [mem]
    $mem_size  - the size of memory [mem] in bytes

    all defintions from [idautils] modules are brought to the environment.

*)

val extract_script : value memmap -> string
