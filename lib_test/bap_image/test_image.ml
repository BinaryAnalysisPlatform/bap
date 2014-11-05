open Core_kernel.Std
open OUnit2
open Or_error
open Word_size

open Bap.Std
open Image_backend

let create_addr = function
  | W32 -> Addr.of_int ~width:32
  | W64 -> Addr.of_int ~width:64

let create_section
    ?(name=".test")
    ?(addr=0)
    ?(perm={r=true; w=true; x=true})
    ?(off=0) ?size ?data asize : Section.t =
  let (size, data) = match size, data with
    | None, None ->  4, String.create 4
    | Some size, None -> size, String.create size
    | None, Some data -> String.length data, data
    | Some size, Some data -> size,data in
  let data = Bigstring.of_string data in
  let addr = create_addr asize addr in
  Section.Fields.create ~name ~addr ~perm ~off ~size ~data

let data ?(base=0) ?(gap=0) f ss asize =
  let addr_ref = ref base in
  List.map ss ~f:(fun data ->
      let data = f data in
      let addr = !addr_ref in
      addr_ref := !addr_ref + String.length data + gap;
      create_section ~addr ~data asize)

let seq (n,m) : string =
  String.init (m-n+1) ~f:(fun i -> Option.value_exn (Char.of_int (i+n)))


let create ?(addr_size=W32) ?(endian=LittleEndian) ~syms ss =
  let sections = Array.of_list (ss addr_size) in
  let symbols = Array.of_list syms in
  let arch = Arch.ARM in
  let entry = create_addr addr_size 0 in
  let of_data _ =
    Some (Img.Fields.create ~arch ~addr_size ~endian ~entry ~sections ~symbols) in
  {
    to_data = None;
    of_data;
  }


let backends =
  let le = LittleEndian and be = BigEndian in
  List.fold ~init:[]
    ~f:(fun acc (n,syms,secs) ->
        (n^"_32LE", create ~addr_size:W32 ~endian:le ~syms secs) ::
        (n^"_32BE", create ~addr_size:W32 ~endian:be ~syms secs) ::
        (n^"_64LE", create ~addr_size:W64 ~endian:le ~syms secs) ::
        (n^"_64BE", create ~addr_size:W64 ~endian:be ~syms secs) ::
        acc) [
    "0-15",  [], data seq [0, 15];
    "16x4",  [], data seq [0, 15; 16,31; 32,47; 48,63];
    "0-15_b",[], data ~base:0x1000FF seq [0, 15];
    "16x4_b",[], data ~base:0x1000FF seq [0, 15; 16,31; 32,47; 48,63];
    "0-15_g",[], data ~gap:0xFD seq [0, 15];
    "16x4_g",[], data ~gap:0xFD seq [0, 15; 16,31; 32,47; 48,63];
  ]

let () = List.iter ~f:(fun (name,backend) ->
    match Image.register_backend ~name backend with
    | `Ok -> ()
    | `Duplicate -> failwith name)
    backends


let print_list r =
  let sexp_of_addr_list = sexp_of_list sexp_of_addr in
  Sexp.to_string_hum @@
  Or_error.sexp_of_t sexp_of_addr_list r


let to_list ~word_size backend ~expect ctxt =
  let r = Image.of_string ~backend ~data:"dummy" >>= fun img ->
    Memory.to_list ~word_size (Image.memory img) in
  let expect = List.map expect ~f:(Addr.of_int ~width:word_size) in
  assert_equal ~ctxt ~printer:print_list (Ok expect) r

let check ?(base=0) backend ~f ctxt =
  let r = Image.of_string ~backend ~data:"dummy"
    >>= fun img -> f img >>= fun _ -> return () in
  let printer err = Sexp.to_string_hum
      (Or_error.sexp_of_t sexp_of_unit err) in
  assert_equal ~ctxt ~msg:"Check failed" ~printer (Ok ()) r


let assert_cont ~word_size img =
  let step = word_size / 8 in
  let base = Image.(Mem.min_addr @@ memory img) in
  Memory.foldi ~word_size (Image.memory img)
    ~init:Addr.(base) ~f:(fun a2 a1 word ->
        (* eprintf "a1 = 0x%LX, a2 = 0x%LX, word = 0x%2LX\n" a1 a2 word; *)
        assert_bool "a1 = a2 -> a1 = base" begin
          Addr.(a1 = a2) ==> Addr.(a1 = base)
        end;
        let s1 = Addr.of_int ~width:32 step in
        let () = match Addr.Int.(!$a2 - !$a1 - !$s1) with
          | Error err -> assert_string @@ Error.to_string_hum err
          | Ok diff ->
            assert_bool "a1 <> a2 -> a2 - a1 = (word_size)"
              (a1 <> a2 ==> Addr.is_zero diff) in
        return a2)

let suite = "Image" >::: [
    "to_list/0-15/bytes_32LE" >:: to_list ~word_size:8 "0-15_32LE"
      ~expect:(List.init 16 ~f:ident);
    "to_list/0-15/bytes_32BE" >:: to_list ~word_size:8 "0-15_32BE"
      ~expect:(List.init 16 ~f:ident);
    "to_list/0-15/bytes_64LE" >:: to_list ~word_size:8 "0-15_64LE"
      ~expect:(List.init 16 ~f:ident);
    "to_list/0-15/bytes_64BE" >:: to_list ~word_size:8 "0-15_64BE"
      ~expect:(List.init 16 ~f:ident);
    "to_list/0-64/bytes_32LE" >:: to_list ~word_size:8 "16x4_32LE"
      ~expect:(List.init 64 ~f:ident);
    "to_list/0-15/int16_32LE" >:: to_list ~word_size:16 "0-15_32LE"
      ~expect:[
        0x0100; 0x0302; 0x0504; 0x0706; 0x0908; 0x0b0a; 0x0d0c; 0x0f0e
      ];
    "to_list/0-15/int16_32BE" >:: to_list ~word_size:16 "0-15_32BE"
      ~expect:[
        0x0001; 0x0203; 0x0405; 0x0607; 0x0809; 0x0a0b; 0x0c0d; 0x0e0f
      ];
    "to_list/0-15/int16_64LE" >:: to_list ~word_size:16 "0-15_64LE"
      ~expect:[
        0x0100; 0x0302; 0x0504; 0x0706; 0x0908; 0x0b0a; 0x0d0c; 0x0f0e
      ];
    "to_list/0-15/int16_64BE" >:: to_list ~word_size:16 "0-15_64BE"
      ~expect:[
        0x0001; 0x0203; 0x0405; 0x0607; 0x0809; 0x0a0b; 0x0c0d; 0x0e0f
      ];
    "to_list/0-15/int64_64LE" >:: to_list ~word_size:64 "0-15_64LE"
      ~expect:[0x0706050403020100; 0x0f0e0d0c0b0a0908];

    "to_list/0-15/int64_64BE" >:: to_list ~word_size:64 "0-15_64BE"
      ~expect:[0x0001020304050607; 0x08090a0b0c0d0e0f];


    "to_list/0-15_b/bytes_32LE" >:: to_list ~word_size:8 "0-15_b_32LE"
      ~expect:(List.init 16 ~f:ident);
    "to_list/0-15_b/bytes_32BE" >:: to_list ~word_size:8 "0-15_b_32BE"
      ~expect:(List.init 16 ~f:ident);
    "to_list/0-15_b/bytes_64LE" >:: to_list ~word_size:8 "0-15_b_64LE"
      ~expect:(List.init 16 ~f:ident);
    "to_list/0-15_b/bytes_64BE" >:: to_list ~word_size:8 "0-15_b_64BE"
      ~expect:(List.init 16 ~f:ident);
    "to_list/0-64/bytes_32LE" >:: to_list ~word_size:8 "16x4_b_32LE"
      ~expect:(List.init 64 ~f:ident);
    "to_list/0-15_b/int16_32LE" >:: to_list ~word_size:16 "0-15_b_32LE"
      ~expect:[
        0x0100; 0x0302; 0x0504; 0x0706; 0x0908; 0x0b0a; 0x0d0c; 0x0f0e
      ];
    "to_list/0-15_b/int16_32BE" >:: to_list ~word_size:16 "0-15_b_32BE"
      ~expect:[
        0x0001; 0x0203; 0x0405; 0x0607; 0x0809; 0x0a0b; 0x0c0d; 0x0e0f
      ];
    "to_list/0-15_b/int16_64LE" >:: to_list ~word_size:16 "0-15_b_64LE"
      ~expect:[
        0x0100; 0x0302; 0x0504; 0x0706; 0x0908; 0x0b0a; 0x0d0c; 0x0f0e
      ];
    "to_list/0-15_b/int16_64BE" >:: to_list ~word_size:16 "0-15_b_64BE"
      ~expect:[
        0x0001; 0x0203; 0x0405; 0x0607; 0x0809; 0x0a0b; 0x0c0d; 0x0e0f
      ];
    "to_list/0-15_b/int64_64LE" >:: to_list ~word_size:64 "0-15_b_64LE"
      ~expect:[0x0706050403020100; 0x0f0e0d0c0b0a0908];

    "to_list/0-15_b/int64_64BE" >:: to_list ~word_size:64 "0-15_b_64BE"
      ~expect:[0x0001020304050607; 0x08090a0b0c0d0e0f];


    "to_list/0-15_g/bytes_32LE" >:: to_list ~word_size:8 "0-15_g_32LE"
      ~expect:(List.init 16 ~f:ident);
    "to_list/0-15_g/bytes_32BE" >:: to_list ~word_size:8 "0-15_g_32BE"
      ~expect:(List.init 16 ~f:ident);
    "to_list/0-15_g/bytes_64LE" >:: to_list ~word_size:8 "0-15_g_64LE"
      ~expect:(List.init 16 ~f:ident);
    "to_list/0-15_g/bytes_64BE" >:: to_list ~word_size:8 "0-15_g_64BE"
      ~expect:(List.init 16 ~f:ident);
    "to_list/0-64/bytes_32LE" >:: to_list ~word_size:8 "16x4_g_32LE"
      ~expect:(List.init 64 ~f:ident);
    "to_list/0-15_g/int16_32LE" >:: to_list ~word_size:16 "0-15_g_32LE"
      ~expect:[
        0x0100; 0x0302; 0x0504; 0x0706; 0x0908; 0x0b0a; 0x0d0c; 0x0f0e
      ];
    "to_list/0-15_g/int16_32BE" >:: to_list ~word_size:16 "0-15_g_32BE"
      ~expect:[
        0x0001; 0x0203; 0x0405; 0x0607; 0x0809; 0x0a0b; 0x0c0d; 0x0e0f
      ];
    "to_list/0-15_g/int16_64LE" >:: to_list ~word_size:16 "0-15_g_64LE"
      ~expect:[
        0x0100; 0x0302; 0x0504; 0x0706; 0x0908; 0x0b0a; 0x0d0c; 0x0f0e
      ];
    "to_list/0-15_g/int16_64BE" >:: to_list ~word_size:16 "0-15_g_64BE"
      ~expect:[
        0x0001; 0x0203; 0x0405; 0x0607; 0x0809; 0x0a0b; 0x0c0d; 0x0e0f
      ];
    "to_list/0-15_g/int64_64LE" >:: to_list ~word_size:64 "0-15_g_64LE"
      ~expect:[0x0706050403020100; 0x0f0e0d0c0b0a0908];

    "to_list/0-15_g/int64_64BE" >:: to_list ~word_size:64 "0-15_g_64BE"
      ~expect:[0x0001020304050607; 0x08090a0b0c0d0e0f];

    "addr/cont/8"  >:: check "0-15_32LE" ~f:(assert_cont ~word_size:8);
    "addr/cont/16" >:: check "0-15_32LE" ~f:(assert_cont ~word_size:16);
    "addr/cont/32" >:: check "0-15_32LE" ~f:(assert_cont ~word_size:32);
    "addr/cont/64" >:: check "0-15_32LE" ~f:(assert_cont ~word_size:64);

    "addr/cont/8"  >:: check "16x4_32LE" ~f:(assert_cont ~word_size:8);
    "addr/cont/16" >:: check "16x4_32LE" ~f:(assert_cont ~word_size:16);
    "addr/cont/32" >:: check "16x4_32LE" ~f:(assert_cont ~word_size:32);
    "addr/cont/64" >:: check "16x4_32LE" ~f:(assert_cont ~word_size:64);

    "addr/cont/8/b"  >:: check "0-15_b_32LE" ~f:(assert_cont ~word_size:8);
    "addr/cont/16/b" >:: check "0-15_b_32LE" ~f:(assert_cont ~word_size:16);
    "addr/cont/32/b" >:: check "0-15_b_32LE" ~f:(assert_cont ~word_size:32);
    "addr/cont/64/b" >:: check "0-15_b_32LE" ~f:(assert_cont ~word_size:64);

    "addr/cont/8/b"  >:: check "16x4_b_32LE" ~f:(assert_cont ~word_size:8);
    "addr/cont/16/b" >:: check "16x4_b_32LE" ~f:(assert_cont ~word_size:16);
    "addr/cont/32/b" >:: check "16x4_b_32LE" ~f:(assert_cont ~word_size:32);
    "addr/cont/64/b" >:: check "16x4_b_32LE" ~f:(assert_cont ~word_size:64);

    "max-min" >:: check "16x4_g_32LE" ~f:(fun img ->
        let m = Image.memory img in
        let (mx,mn) = Memory.(max_addr m, min_addr m) in
        let printer = Addr.to_string in
        assert_equal ~printer Addr.(mn ++ 0x336) mx;
        return ());

    "view" >:: check "16x4_b_32LE" ~f:(fun img ->
        let m = Image.memory img in
        let from = Addr.of_int ~width:32 0x100103 in
        Memory.view m ~from ~word_size:8 ~words:20 >>= fun m ->
        let expect = List.init 20 ~f:(fun i -> i + 4) |>
                     List.map ~f:(Word.of_int ~width:8) in
        assert_equal ~printer:print_list
          (return expect)
          (Memory.to_list ~word_size:8 m);
        let (mx,mn) = Memory.(max_addr m, min_addr m) in
        assert_equal Addr.(of_int ~width:32 0x100103) mn;
        assert_equal Addr.(of_int ~width:32 0x100116) mx;
        return ())
  ]
