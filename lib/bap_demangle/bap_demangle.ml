open Core_kernel[@@warning "-D"]
open Bap_core_theory

module Std = struct
  type demangler = {
    name : KB.Name.t;
    run : string -> string
  }

  let registry = Hashtbl.create (module KB.Name)
  let selected = Hashtbl.create (module Theory.Target)

  module Demangler = struct
    type t = demangler
    let create ?package name run =
      let name = KB.Name.create ?package name in
      if Hashtbl.mem registry name
      then failwithf "The demangler %s is already registered, \
                      please pick a unique name" (KB.Name.show name) ();
      Hashtbl.add_exn registry name run;
      {name; run}

    let declare ?package name run = ignore (create ?package name run)

    let run d = d.run
    let fullname d = d.name
    let name d = KB.Name.unqualified d.name

    let id = create ~package:"bap" "id" Fn.id
    let strip_leading_underscore =
      create ~package:"bap" "strip-leading-underscore" @@ fun s ->
      match String.chop_prefix s ~prefix:"_" with
      | None -> s
      | Some s -> s
  end

  module Demanglers = struct
    let register = ignore
    let available () =
      Hashtbl.to_alist registry |>
      List.map ~f:(fun (name,run) -> {name; run})

    let install target demangler =
      match Hashtbl.add selected target demangler with
      | `Ok -> ()
      | `Duplicate ->
        let used = Hashtbl.find_exn selected target in
        failwithf "Failed to install demangler %s to the \
                   target %s, which already has the demangler %s."
          (KB.Name.show used.name)
          (KB.Name.show (Theory.Target.name target))
          (KB.Name.show demangler.name)
          ()

    let bad_name name =
      let names =
        available () |>
        List.map ~f:(fun d -> KB.Name.show (Demangler.fullname d)) |>
        String.concat ~sep:", " in
      invalid_argf "Failed to find a demangler named %s, \
                    the list of available demanglers: %s"
        (KB.Name.show name) names ()


    let get ?package name =
      let name = KB.Name.create ?package name in
      match Hashtbl.find registry name with
      | Some run -> {name; run}
      | None -> bad_name name

    let select target = match Hashtbl.find selected target with
      | None -> Demangler.id
      | Some d -> d

    let lookup ?package name =
      try Some (get ?package name) with _ -> None
  end


end
