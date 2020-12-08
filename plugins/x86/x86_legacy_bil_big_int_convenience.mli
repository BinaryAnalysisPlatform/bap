(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)

val bi0 : Z.t
val bi1 : Z.t
val bi2 : Z.t
val bi3 : Z.t
val bi4 : Z.t
val bi5 : Z.t
val bi6 : Z.t
val bi7 : Z.t
val bi8 : Z.t
val bi9 : Z.t
val bia : Z.t
val bib : Z.t
val bic : Z.t
val bid : Z.t
val bie : Z.t
val bif : Z.t
val bim1 : Z.t
val bim2 : Z.t
val bim3 : Z.t
val bim4 : Z.t
val bim5 : Z.t
val bim6 : Z.t
val bim7 : Z.t
val bim8 : Z.t
val bim9 : Z.t
val bima : Z.t
val bimb : Z.t
val bimc : Z.t
val bimd : Z.t
val bime : Z.t
val bimf : Z.t
val biconst : int -> Z.t
val bi : int -> Z.t
val biconst32 : int32 -> Z.t
val bi32 : int32 -> Z.t
val biconst64 : int64 -> Z.t
val bi64 : int64 -> Z.t
val bimask8 : Z.t
val bimask16 : Z.t
val bimask32 : Z.t
val bimask64 : Z.t
val bimask128 : Z.t
val big_int_of_bool : bool -> Z.t
val ( ==% ) : Z.t -> Z.t -> bool
val ( <>% ) : Z.t -> Z.t -> bool
val ( <% ) : Z.t -> Z.t -> bool
val ( <=% ) : Z.t -> Z.t -> bool
val ( >% ) : Z.t -> Z.t -> bool
val ( >=% ) : Z.t -> Z.t -> bool
val ( <<% ) : Z.t -> int -> Z.t
val ( >>% ) : Z.t -> int -> Z.t
val ( $>>% ) : Z.t -> int -> Z.t
val ( +% ) : Z.t -> Z.t -> Z.t
val ( *% ) : Z.t -> Z.t -> Z.t
val ( ++% ) : Z.t -> Z.t
val ( -% ) : Z.t -> Z.t -> Z.t
val ( |% ) : Z.t -> Z.t -> Z.t
val ( &% ) : Z.t -> Z.t -> Z.t
val ( /% ) : Z.t -> Z.t -> Z.t
val ( %% ) : Z.t -> Z.t -> Z.t
val ( ~% ) : Z.t -> string
val bi_is_zero : Z.t -> bool
val bi_is_one : Z.t -> bool
val bi_is_minusone : Z.t -> bool
val uintmax64 : Z.t
val sintmax64 : Z.t
val addr_to_int64 : Z.t -> int64
val addr_of_int64 : int64 -> Z.t
type address = Big_int_Z.big_int
val address_to_int64 : address -> int64
val address_of_int64 : int64 -> address
val big_int_to_hex : ?pad:int -> Z.t -> string
