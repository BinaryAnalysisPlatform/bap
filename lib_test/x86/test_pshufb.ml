(**
# rappel (don't forget -x option)
## mov to xmm0 register a 128-bit value "51 50 4F 4E 4D 4C 4B 4A 49 48 47 46 45 44 43 42"

    mov rax, 0x4948474645444342
    mov rbx, 0x51504F4E4D4C4B4A
    pinsrq xmm0, rax, 0
    pinsrq xmm0, rbx, 1

So, xmm0 will be used for original value, xmm1 - for mask, and xmm2
for our tests.

Cases:
1) mov to xmm1 register a 128 bit value with permutation indexes, just
   bytes numbers as they are, i.e "0F 0E 0D 0C 0B 0A 09 08 07 06 05 04 03 02 01 00"
   expected: xmm2 value should be the same as before

    mov rbx, 0x0706050403020100
    mov rcx, 0x0f0e0d0c0b0a0908
    pinsrq xmm1, rbx, 0
    pinsrq xmm1, rcx, 1
    pshufb xmm2, xmm1

2) permutation mask = "0F 0E 0D 0C 0B 0A 09 08 07 06 05 04 03 01 01 01",
   i.e. last three bytes of destination should be the same as byte #01.
   expected: xmm2 = "51 50 4F 4E 4D 4C 4B 4A 49 48 47 46 45 43 43 43"

   movaps xmm2, xmm0
   mov rbx, 0x0706050403010101
   mov rcx, 0x0f0e0d0c0b0a0908
   pinsrq xmm1, rbx, 0
   pinsrq xmm1, rcx, 1
   pshufb xmm2, xmm1

3) permutation mask = "01 01 01 0C 0B 0A 09 08 07 06 05 04 03 02 01 00",
   i.e. first three bytes of destination should be the same as byte #01.
   expected: xmm2 = "43 43 43 4E 4D 4C 4B 4A 49 48 47 46 45 44 43 42"

   movaps xmm2, xmm0
   mov rbx, 0x0706050403020100
   mov rcx, 0x0101010c0b0a0908
   pinsrq xmm1, rbx, 0
   pinsrq xmm1, rcx, 1
   pshufb xmm2, xmm1

4) permutation mask = "0F 0E 0D 0C 0B 0A 09 08 07 06 05 04 03 80 80 80",
   i.e. last three bytes of destination shold be eqaul to zero.
   expected: xmm2 = "51 50 4F 4E 4D 4C 4B 4A 49 48 47 46 45 00 00 00"

   movaps xmm2, xmm0
   mov rbx, 0x0706050403808080
   mov rcx, 0x0f0e0d0c0b0a0908
   pinsrq xmm1, rbx, 0
   pinsrq xmm1, rcx, 1
   pshufb xmm2, xmm1

*)

open Core_kernel.Std
open Bap.Std
open OUnit2

let mktemp t = Var.create ~fresh:true ~is_virtual:true "tmp" t

let word_size = 64
let op_size = 128
let op_typ = Type.imm op_size

let bil dst src =
  let foreach_size f = List.concat @@ List.init (op_size / 8) ~f in
  let index_bits = 4 in

  let byte_typ = Type.imm 8 in
  let zero = Bil.int @@ Word.zero op_size in
  let byte = Bil.int @@ Word.of_int ~width:8 8 in
  let msb_one = Bil.int @@ Word.of_int ~width:8 0x80 in

  let tmp_dst = mktemp op_typ in
  let tmp_byte = mktemp op_typ in
  let iv = mktemp byte_typ in
  let mask_byte_i = mktemp byte_typ in
  let ind = mktemp byte_typ in

  List.concat [
    [Bil.move tmp_dst zero];
    foreach_size (fun i ->
        Bil.[
          iv := int (Word.of_int ~width:8 i);
          mask_byte_i := extract 7 0 (var src lsr (var iv * byte));
          if_ (msb_one land var mask_byte_i = msb_one) [
            tmp_byte := zero;
	  ] (* else *) [
	    ind := cast unsigned 8 (extract index_bits 0 (var mask_byte_i));
            tmp_byte :=
              cast unsigned op_size (extract 7 0 (var dst lsr (var ind * byte)))
          ];
          tmp_dst := Bil.(var tmp_dst lor (var tmp_byte lsl (var iv * byte)));
        ]);
    [Bil.move dst (Bil.var tmp_dst);]
  ]

let of_bytes s =
  let str = String.filter ~f:(fun c -> Char.(c <> ' ')) s in
  Word.of_string @@ sprintf "0x%s:128u" str

let origin = of_bytes "51 50 4F 4E 4D 4C 4B 4A 49 48 47 46 45 44 43 42"

let test (mask, expected) ctxt =
  let c = new Bili.context in
  let xmm0 = mktemp op_typ in
  let xmm1 = mktemp op_typ in
  let bil = Bil.[
      move xmm0 (int origin);
      move xmm1 (int mask);
    ] @ bil xmm0 xmm1 in
  let c = Stmt.eval bil c in
  assert_bool "got wrong result" @@
  match c#lookup xmm0 with
  | None -> false
  | Some r ->
    match Bil.Result.value r with
    | Bil.Imm word -> Word.equal word expected
    | _ -> false

let no_permutation =
  of_bytes "0F 0E 0D 0C 0B 0A 09 08 07 06 05 04 03 02 01 00",
  origin

let last_three_0x43 =
  of_bytes "0F 0E 0D 0C 0B 0A 09 08 07 06 05 04 03 01 01 01",
  of_bytes "51 50 4F 4E 4D 4C 4B 4A 49 48 47 46 45 43 43 43"

let first_three_0x43 =
  of_bytes "01 01 01 0C 0B 0A 09 08 07 06 05 04 03 02 01 00",
  of_bytes "43 43 43 4E 4D 4C 4B 4A 49 48 47 46 45 44 43 42"

let last_three_0x00 =
  of_bytes "0F 0E 0D 0C 0B 0A 09 08 07 06 05 04 03 80 80 80",
  of_bytes "51 50 4F 4E 4D 4C 4B 4A 49 48 47 46 45 00 00 00"

let suite () =
  "X86.pshufb" >::: [
    "no permuatation"           >::  test no_permutation;
    "last three bytes = 0x43"   >::  test last_three_0x43;
    "first three bytes = 0x43"  >::  test first_three_0x43;
    "last three bytes = 0x0"    >::  test last_three_0x00;
  ]
