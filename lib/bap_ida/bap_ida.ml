open Core_kernel.Std

module Command = struct
  type 'a t = {
    script  : string;
    parser : string -> 'a;
    language : [`python | `idc ]
  } [@@deriving fields]

  type language = [`python | `idc]
  let create language = Fields.create ~language
end

type 'a command = 'a Command.t

module Service = struct
  type t = {
    exec : 'a. 'a command -> 'a;
    close : unit -> unit
  }

  exception Service_not_provided

  let creator = ref (fun _ -> {
        exec = (fun x -> raise Service_not_provided);
        close = (fun () -> raise Service_not_provided);
      } )

  let create target : t = !creator target
  let provide (create:string -> t) : unit = creator := create

end

module Ida = struct
  type t = Service.t

  open Service

  exception Failed of string
  exception Not_in_path

  let create = Service.create
  let exec service = service.exec
  let close service = service.close ()

  let with_file target command =
    let ida = create target in
    let f ida = exec ida command in
    protectx ~f ida ~finally:close
end

module Std = struct
  type ida = Ida.t
  type 'a command = 'a Command.t
  module Ida = Ida
  module Command = Command
  module Service = Service
end
