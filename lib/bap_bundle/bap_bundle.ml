open Core_kernel

module Filename = Caml.Filename

module Std = struct
  exception Not_a_bundle


  module Manifest = struct
    let getenv key = try Sys.getenv key with Caml.Not_found -> "unknown"

    type t = {
      name : string;
      version : string;
      desc : string;
      main : string;
      author : string;
      date : float;
      requires : string sexp_list;
      provides : string sexp_list;
      url : string sexp_option;
      license : string sexp_option;
      copyrights : string sexp_option;
      tags : string sexp_list;
      cons : string sexp_list;
    } [@@deriving bin_io, compare, fields, sexp]

    let create
        ?(author=getenv "USER")
        ?(version="1.0.0")
        ?main
        ?(date=Unix.time ())
        ?(desc = "description not provided")
        ?(requires=[])
        ?(provides=[])
        ?url ?license ?copyrights
        ?(tags=[])
        ?(cons=[]) name = {
      name; author; version; date; desc; requires; provides;
      copyrights; license; url; tags; cons;
      main = Option.value main ~default:name;
    }

    let pp_sexp ppf = function
      | Sexp.Atom _
      | Sexp.List [] -> ()
      | Sexp.List (x :: xs) ->
        let open Format in
        let pp = Sexp.pp_hum_indent 1 in
        fprintf ppf "@[<v1>(%a" pp x;
        List.iter xs ~f:(fun x ->
            fprintf ppf "@;%a" pp x);
        fprintf ppf ")@]"

    let pp ppf m =
      let sexp = sexp_of_t m in
      Format.fprintf ppf "%a" pp_sexp sexp

    let to_string m =
      Format.asprintf "%a" pp m

    let of_string s = t_of_sexp (Sexp.of_string s)

  end

  type manifest = Manifest.t

  module Nameof = struct
    let manifest = "MANIFEST.scm"
  end


  module Bundle  = struct
    type filename = string

    type bundle = {
      name : string;
      path : filename;
    }
    type t = bundle

    let main =
      let name = Filename.basename Sys.executable_name in
      ref {
        name;
        path = name ^ ".bundle"
      }

    let open_in uri =
      try Zip.open_in uri with
      | Zip.Error _ -> raise Not_a_bundle


    module Builder = struct
      type t = {
        mutable manifest : manifest option;
        mutable files : (string option * Uri.t) list;
        mutable data  : ([`Name of string] * string) list;
      }

      let create () = {
        manifest = None;
        files = [];
        data = [];
      }

      let embed_manifest b manifest =
        b.manifest <- Some manifest

      let put_file ?name b uri =
        b.files <- (name,uri) :: b.files

      let put_data b ~name ~data =
        b.data <- (`Name name,data) :: b.data

      let flush b uri =
        let path = Uri.path uri in
        let zip = Zip.open_out path in
        List.iter b.files ~f:(fun (name,uri) ->
            let path = Uri.path uri in
            let name = Option.value name ~default:path in
            Zip.copy_file_to_entry path zip name);
        List.iter b.data ~f:(fun (`Name name,data) ->
            Zip.add_entry data zip name);
        let man = match b.manifest with
          | Some man -> man
          | None -> Manifest.create "noname" in
        let mdata = Manifest.to_string man in
        Zip.add_entry mdata zip Nameof.manifest;
        Zip.close_out zip
    end

    let input b f =
      let zip = open_in b.path in
      protect ~f:(fun () -> f zip)
        ~finally:(fun () -> Zip.close_in zip)

    let (>>>) = input

    let manifest b =
      b >>> fun zip ->
      let data = Zip.read_entry zip (Zip.find_entry zip Nameof.manifest) in
      Manifest.of_string data

    let of_uri uri =
      let path = Uri.path uri in
      let base = Filename.basename path in
      let name = if String.mem base '.'
        then Filename.chop_extension base else base in
      {name; path}

    let get_file ?name b uri =
      b >>> fun zip -> try
        let path = Uri.path uri in
        let entry = Zip.find_entry zip path in
        let name = Option.value name ~default:path in
        Zip.copy_entry_to_file zip entry name;
        Some (Uri.of_string name)
      with Caml.Not_found -> None

    let get_data b name =
      b >>> fun zip ->
      try Some Zip.(read_entry zip (find_entry zip name))
      with Caml.Not_found -> None

    let list b =
      b >>> fun zip ->
      Zip.entries zip |> List.filter_map ~f:(fun e ->
          let name = Zip.(e.filename) in
          Option.some_if (not (String.equal name Nameof.manifest)) name)

    let transform files bundle ~f =
      let zin = open_in bundle.path in
      let store filename data =
        Hashtbl.set files ~key:filename ~data in
      Zip.entries zin |> List.iter ~f:(fun entry ->
          let filename = Zip.(entry.filename) in
          let process_file f =
            let name,chan =
              Filename.open_temp_file "bundle" "entry" in
            Zip.copy_entry_to_channel zin entry chan;
            Out_channel.close chan;
            f name;
            store filename (`Move name) in
          let process_data f =
            let data = Zip.read_entry zin entry in
            store filename (`Data (f data)) in
          match f filename with
          | `Map f -> process_data f
          | `Proc f -> process_file f
          | `Copy -> process_file ignore
          | `Drop -> ());
      Zip.close_in zin;
      let zout = Zip.open_out bundle.path in
      Hashtbl.iteri files ~f:(fun ~key:name ~data ->
          match data with
          | `Data s -> Zip.add_entry s zout name
          | `Copy f -> Zip.copy_file_to_entry f zout name
          | `Move f -> Zip.copy_file_to_entry f zout name;
            Sys.remove f);
      Zip.close_out zout

    let update bundle ~f =
      transform (String.Table.create ()) bundle ~f

    let insert bundle files =
      let files = String.Table.of_alist_exn files in
      transform files bundle ~f:(fun _ -> `Copy)

    let insert_files bundle files =
      insert bundle (List.map files ~f:(fun (name,uri) ->
          let path = Uri.path uri in
          let name = Option.value name ~default:path in
          name,`Copy path))

    let insert_chunks bundle data =
      insert bundle (List.map data ~f:(fun (`Name name,data) ->
          name,`Data data))

    let insert_file ?name bundle file =
      insert_files bundle [name,file]

    let insert_data bundle ~name ~data =
      insert_chunks bundle [`Name name, data]

    let update_manifest bundle ~f =
      update bundle ~f:(fun file ->
          if String.equal file Nameof.manifest
          then `Map (fun s -> Manifest.(of_string s |> f |> to_string))
          else `Copy)

  end
  type bundle = Bundle.t

  let set_main_bundle bundle = Bundle.main := bundle
  let main_bundle () = Bundle.main.contents
end
