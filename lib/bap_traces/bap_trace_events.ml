open Core_kernel.Std
open Regular.Std
open Bap.Std
open Bap_trace_event_types

let pp_bytes fmt s =
  let pp fmt s =
    String.iter ~f:(fun c -> Format.fprintf fmt "%X@ " (Char.to_int c)) s in
  Format.fprintf fmt "@[<hv>%a@]" pp s

let pp_array pp fmt ar =
  let ppa pref = Format.fprintf fmt "%s%a" pref pp in
  ignore(Array.fold ~f:(fun pref a -> ppa pref a; ", ") ~init:"" ar)

module Move = struct
  include Move
  let pp pp_cell arr ppf t =
    Format.fprintf ppf "%a %s %a" pp_cell t.cell arr Word.pp t.data
end

module Load = struct
  type t = addr move [@@deriving bin_io, compare, sexp]
  let pp = Move.pp Addr.pp "=>"
end

module Store = struct
  type t = addr move [@@deriving bin_io, compare, sexp]
  let pp = Move.pp Addr.pp "<="
end

module Read = struct
  type t = var move [@@deriving bin_io, compare, sexp]
  let pp = Move.pp Var.pp "=>"
end

module Write = struct
  type t = var move [@@deriving bin_io, compare, sexp]
  let pp = Move.pp Var.pp "<="
end

module Chunk = struct
  include Chunk
  let pp ppf t =
    Format.fprintf ppf "@@%a: %a" Addr.pp t.addr pp_bytes t.data
end

module Syscall = struct
  include Syscall
  let pp ppf s =
    Format.fprintf ppf "syscall[%d](%a)" s.number (pp_array Word.pp) s.args
end

module Exn = struct
  include Exn
  let pp ppf s =
    let ppo pref ppf = function
      | Some a -> Format.fprintf ppf " %s %a" pref Addr.pp a
      | None -> () in
    Format.fprintf ppf "exn %d%a%a"
      s.number (ppo "from") s.src (ppo "to") s.dst
end

module Location = struct
  include Location
  let pp ppf t =
    let ppo ppf = function
      | Some a -> Format.fprintf ppf "%s@" a
      | None -> () in
    Format.fprintf ppf "%a %a" ppo t.name Addr.pp t.addr
end

module Call = struct
  include Call
  let pp ppf s =
    Format.fprintf ppf "%a -> %a(%a)"
      Location.pp s.caller Location.pp s.callee (pp_array Addr.pp) s.args
end

module Return = struct
  include Return
  let pp ppf s = Format.fprintf ppf "%s <- %s" s.caller s.callee
end

module Modload = struct
  include Modload
  let pp fmt t =
    Format.fprintf fmt "%s: %a - %a" t.name Addr.pp t.low Addr.pp t.high
end

let memory_load =
  Value.Tag.register (module Load)
    ~name:"memory-load"
    ~uuid:"9546a981-de85-4e5c-8d59-73a15bf5c7bd"

let memory_store =
  Value.Tag.register (module Store)
    ~name:"memory-store"
    ~uuid:"d5995d83-76be-410d-94a9-b0cfcb91f2de"

let register_read =
  Value.Tag.register (module Read)
    ~name:"register-read"
    ~uuid:"ded5dc91-dafc-4316-9c6c-4dad4e40a273"

let register_write =
  Value.Tag.register (module Write)
    ~name:"register-write"
    ~uuid:"395f5f37-5aed-4bd2-a51f-1c7216b5cd7c"

let timestamp =
  Value.Tag.register (module Int64)
    ~name:"timestamp"
    ~uuid:"0feea5c2-b471-48e4-a10f-c7e18cbf21a9"

let pc_update =
  Value.Tag.register (module Addr)
    ~name:"pc-update"
    ~uuid:"98ea397e-d726-43be-9ec5-bf226d67578f"

let code_exec =
  Value.Tag.register (module Chunk)
    ~name:"code-exec"
    ~uuid:"b8b3af3a-d1aa-4bf0-a36f-4ea6d0dd2bbf"

let context_switch =
  Value.Tag.register (module Int)
    ~name:"context-switch"
    ~uuid:"7f1d322a-d2cc-4e42-8e7a-081080751268"

let syscall =
  Value.Tag.register (module Syscall)
    ~name:"syscall"
    ~uuid:"6e0eec5c-2907-4c4c-b9b1-b879d2cbc69b"

let exn =
  Value.Tag.register (module Exn)
    ~name:"exn"
    ~uuid:"18ae62d6-aa66-429b-964c-e15b7913d57e"

let call =
  Value.Tag.register (module Call)
    ~name:"call"
    ~uuid:"fe9899fe-3b60-4d10-bb50-dbccfc4ee0da"

let return =
  Value.Tag.register (module Return)
    ~name:"return"
    ~uuid:"2cae388a-69cb-48a0-8355-d3f9d39ac8eb"

let modload =
  Value.Tag.register (module Modload)
    ~name:"modload"
    ~uuid:"7f842d03-6c9f-4745-af39-002f468f7fc8"
