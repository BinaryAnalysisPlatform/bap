open Core_kernel
open Format

type stage = Working | Accepted | Finished [@@deriving compare]
type answer = Accept | Decline | Undecided

type +'a decision = {
  m : int;                    (* number of e0 events *)
  n : int;                    (* number of e1 events *)
  cut : int;                  (* length of the tail *)
  len : int;                  (* length of chars and data *)
  chars : char list;
  data : 'a list;
  stage : stage;
}

type +'a t = {
  t0 : float;                 (* lower (decline) threshold *)
  t1 : float;                 (* upper (accept) threshold *)
  w0 : float;                 (* w(e1|h1) *)
  w1 : float;                 (* w(e2|h2) *)
  p1 : float;                 (* a prior probability of text *)
  h  : 'a decision;         (* current hypothesis *)
  alphabet : Char.Set.t;      (* alphabet *)
}

let empty = {
  chars = []; data = [];
  n = 0; m = 0; cut = 0; len = 0; stage = Working;
}

let log1p = Float.log1p

let create
    ?(alpha=0.05)
    ?(beta=0.001)
    ?(p1=0.5)
    ?(ps=0.05)
    ?len_pdf alphabet =
  let s = 245. in
  let a = float (Set.length alphabet) in
  let n = 1. /. ps in {
    alphabet;
    t0 = log beta; t1 = log1p ~-.alpha;
    p1 = log p1;
    w0 = log1p ~-.ps;
    w1 = log (a *. n +. s -. a) -. log (a *. n);
    h = empty;
  }

let hprop {p1; w0; w1; h={n;m}} =
  float m *. w0 +. float n *. w1 +. p1

let decide t p =
  if Float.(p <= t.t0) then Decline else
  if Float.(p >= t.t1) then Accept else Undecided

let push h x c = {
  h with
  len = h.len + 1;
  cut = if Caml.(h.stage = Accepted) then h.cut + 1 else h.cut;
  chars = c :: h.chars;
  data = x :: h.data;
}

let update t x char =
  let e1 = Set.mem t.alphabet char in
  let h = {
    t.h with
    n = if e1 then t.h.n + 1 else t.h.n;
    m = if e1 then t.h.m else t.h.m + 1;
  } in
  if e1
  then {t with h = push h x char}
  else {t with h}

let step_char t x char =
  let t = update t x char in
  let p = hprop t in
  match t.h.stage,decide t p with
  | Accepted,Accept -> assert false
  | Accepted,Undecided when Float.(p > t.p1) ->
    {t with h = {t.h with n = 0; m = 0; cut=0}}
  | Accepted,Undecided -> t
  | Accepted,Decline -> {t with h = {t.h with stage = Finished}}
  | Working, Accept ->
    {t with h = {t.h with stage=Accepted; n = 0; m = 0}}
  | Working,Undecided when t.h.n = 0 -> {t with h = empty}
  | Working,Decline -> {t with h = empty}
  | Working,Undecided -> t
  | Finished,_ -> t

let step t x char =
  if [%compare.equal: stage] t.h.stage Finished
  then step_char {t with h = empty} x char
  else step_char t x char


let when_decided t ~f init =
  if [%compare.equal: stage] t.h.stage Finished then f t.h else init

let decision t =
  when_decided t ~f:(fun x -> Some x) None

let abort t =
  if [%compare.equal: stage] t.h.stage Accepted
  then Some {t.h with cut=0}
  else None


let run t =
  let open Sequence.Step in
  Sequence.unfold_with ~init:t ~f:(fun t (x,c) ->
      let t = step t x c in
      when_decided t (Skip t) ~f:(fun d -> Yield (d,t)))


let rev_zip_skip skip xs ys =
  let rec loop xs ys zs r = match xs,ys with
    | x::xs,y::ys -> if r > 0
      then loop xs ys zs (r-1)
      else loop xs ys ((x,y)::zs) 0
    | [],[] -> zs
    | _ -> invalid_arg "data and chars should be equal in size" in
  loop xs ys [] skip

let result {data; chars; cut} = rev_zip_skip cut data chars

let chars {chars; cut; len} =
  let data = Bytes.create (len - cut) in
  let len = Bytes.length data in
  List.iteri chars ~f:(fun i c ->
      if i >= cut
      then Bytes.set data ((len - 1) - (i - cut))  c);
  Bytes.to_string data

let data ?(rev=false) {data; cut} =
  if rev then List.drop data cut
  else List.drop data cut |> List.rev

let pp_stats ppf t =
  fprintf ppf
    "%.2f < p < %.2f where p = %.2f = %d*%.2f - %d * %.2f - %.2f"
    t.t0 t.t1 (hprop t) t.h.n t.w1 t.h.m (-. t.w0) (-. t.p1)

let string_of_stage = function
  | Working  -> "W"
  | Accepted -> "A"
  | Finished -> "F"

let pp_decision ppf h  =
  fprintf ppf "%s: %S" (string_of_stage h.stage) (chars h)


let pp ppf t = pp_decision ppf t.h
