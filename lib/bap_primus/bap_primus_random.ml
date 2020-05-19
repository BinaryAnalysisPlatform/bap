open Core_kernel
module Iterator = Bap_primus_iterator
module type S = Iterator.Infinite.S

(* Taken from arXiv:2001.05304   *)
let mcg16 = "0x7dc5"
let mcg32 = "0xae3cc725"
let mcg64 = "0xcc62fceb9202faad"
let mcg128 = "0xace2628409311ff16a545ebdff0d414d"

(* Multiplicative Congruental Generator with power of two moduli.

   A generator with modulus m=2^s generates s-3 bits of
   pseudorandomness with the period m/4, given that the seed is an odd
   number.

   To build an p-bit generator we use k r-bit sub-generators, where k
   is $ceil (p / (r-3))$. Each generator is having a different seed
   obtained from the seed provided to the p-bit generator.
*)
module MCG = struct
  module type Size = sig
    val size : int
  end
  module type S = sig
    include S with type dom = Bitvec.t
    include Size
    val create : int -> t
  end

  module type Z = Bitvec.S with type 'a m = 'a

  let ceil_div m n = (m + n - 1) / n
  let odd_hash seed = max (Hashtbl.hash seed * 2 + 1) 1

  module Make(Z : Z)(P : sig val m : int val a : string end) : S = struct
    module Z = Z
    let a = Bitvec.of_string P.a
    let size = P.m - 3
    type t = Bitvec.t
    type dom = Bitvec.t

    let noise = Z.int 3
    let min = Z.zero
    let max = Z.(ones lsr noise)
    let value x = Z.(x lsr noise)
    let next x : t = Z.(a * x)
    let create seed = next @@ Z.int @@ odd_hash seed

  end

  module Bitvec_M16 =
    Bitvec.Make(struct let modulus = Bitvec.modulus 16 end)

  (* Pre: Output.Size > Sub.size *)
  module Cat(Output : Size)(Sub : S) : S = struct
    type t = Sub.t array
    type dom = Bitvec.t

    module Z = Bitvec.Make(struct
        let modulus = Bitvec.modulus Output.size
      end)

    let size = Output.size
    let min = Bitvec.zero
    let max = Z.ones
    let generators = ceil_div Output.size Sub.size
    let noise = Output.size - generators

    let create seed : t =
      Array.init generators (fun salt ->
          Sub.create (seed + salt))

    let next = Array.map ~f:Sub.next

    let value xs =
      Array.fold ~f:(fun y x ->
          Z.(y lsl int Sub.size lor Sub.value x)) ~init:Z.zero xs
  end


  (* Pre: Output.Size < Sub.size  *)
  module Cut(Output : Size)(Sub : S) : S = struct
    type t = Sub.t
    type dom = Sub.dom
    module Z = Bitvec.Make(struct
        let modulus = Bitvec.modulus Output.size
      end)
    let min = Bitvec.zero
    let max = Z.ones
    let size = Output.size
    let noise = Z.int @@ Sub.size - Output.size
    let next = Sub.next
    let value x = Z.(Sub.value x lsr noise)
    let create = Sub.create
  end

  module type Range = sig
    include Size
    val min : Bitvec.t
    val max : Bitvec.t
  end


  (* pre:
     - R.size >= Gen.size
     - Gen.size = 2^(max-min+1)
     - Gen.min = 0
     - Gen.max = 2^Gen.size
  *)
  module Range(R : Range)(Gen : S) = struct
    include Gen
    module Z = Bitvec.Make(struct
        let modulus = Bitvec.modulus R.size
      end)
    let value x =
      let u = Gen.value x in
      Z.(R.min + u % (R.max-R.min+one))
    let min = R.min
    let max = R.max
    let size = R.size
  end

  (* Basis Generators *)
  module M13 = Make(Bitvec_M16)(struct let m = 16 let a = mcg16 end)
  module M29 = Make(Bitvec.M32)(struct let m = 32 let a = mcg32 end)
  module M61 = Make(Bitvec.M64)(struct let m = 64 let a = mcg64 end)

  (* Common Generators *)
  module M8  = Cut(struct let size = 8 end)(M13)
  module M16 = Cut(struct let size = 16 end)(M29)
  module M32 = Cut(struct let size = 32 end)(M61)
  module M64 = Cat(struct let size = 64 end)(M29)

  let tabulate (module Gen : S) ~n seed =
    let rec loop i g =
      if i < n then begin
        Format.printf "%3d: %a@\n" i Bitvec.pp (Gen.value g);
        loop (i+1) (Gen.next g)
      end in
    loop 0 (Gen.create seed)

  let tabulate_ints (module Gen : S) ~n seed =
    let rec loop i g =
      if i < n then begin
        Format.printf "%3d: %2ld@\n" i (Bitvec.to_int32 (Gen.value g));
        loop (i+1) (Gen.next g)
      end in
    loop 0 (Gen.create seed)

  let cut m (module G : S) : (module S) =
    (module Cut(struct let size = m end)(G))

  let cat m (module G : S) : (module S) =
    (module Cat(struct let size = m end)(G))

  let make_for_width : int -> (module S) = function
    | 8  -> (module M8)
    | 13 -> (module M13)
    | 16 -> (module M16)
    | 29 -> (module M29)
    | 32 -> (module M32)
    | 64 -> (module M64)
    | n when n < 13 -> cut n (module M13)
    | n when n < 29 -> cut n (module M29)
    | n when n < 61 -> cut n (module M61)
    | n -> cat n (module M61)

  let create ?min ?max width =
    if Option.is_none min && Option.is_none max
    then make_for_width width
    else
      let module Z = Bitvec.Make(struct
          let modulus = Bitvec.modulus width
        end) in
      let module R = struct
        let min = match min with
          | None -> Bitvec.zero
          | Some min -> min
        let max = match max with
          | None -> Z.ones
          | Some max -> max
        let size = width
      end in
      let bits = Bitvec.to_int Z.(R.max - R.min + one) in
      let module Gen = (val make_for_width bits) in
      (module Range(R)(Gen))

  let create_small ?min ?max width =
    let int x = Bitvec.(int x mod modulus width) in
    let min = Option.map ~f:int min
    and max = Option.map ~f:int max in
    create ?min ?max width
end
