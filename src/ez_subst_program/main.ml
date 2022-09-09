(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2022 OCamlPro & Origin Labs                             *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open EzCompat
open Ez_subst.V2
open Ez_file.V1
open EzFile.OP

type subst = {
  mutable map : string option StringMap.t ;
  mutable active : bool ;
  mutable inherits : bool ;
}

type config = {
  mutable sep : char option ;
  mutable escape : bool option ;
  mutable sym : bool option ;
  mutable sym_fail : bool option ;

  mutable var : subst option ;
  mutable paren : subst option ;
  mutable bracket : subst option ;
  mutable brace : subst option ;
}

type task = {
  loc : string ;
  src : string ;
  dst : string option ;
  config : config ;
}

let new_config () = {
  sep = None;
  escape = None ;
  sym = None ;
  sym_fail = None ;

  var = None ;
  paren = None ;
  brace = None ;
  bracket = None ;
}

let new_subst () = {
  inherits = true ;
  active = true ;
  map = StringMap.empty ;
}

let merge_substs s0 s1 =
  match s1 with
  | None ->
      begin
        match s0 with
        | None -> None
        | Some s0 ->
            if s0.active then Some s0 else None
      end
  | Some s1 ->
      if s1.active then
        if s1.inherits then
          match s0 with
          | None -> Some s1
          | Some s0 ->
              if s0.active then
                Some {
                  active = true ;
                  inherits = s0.inherits ;
                  map = StringMap.union (fun _k _v0 v1 -> Some v1)
                      s0.map s1.map ;
                }
              else
                Some s1
        else
          Some s1
      else
        None

let merge_options o1 o2 =
  match o2 with
  | None -> o1
  | Some _ -> o2

let merge_configs c0 c1 =
  {
    escape = merge_options c0.escape c1.escape ;
    sym = merge_options c0.sym c1.sym ;
    sym_fail = merge_options c0.sym_fail c1.sym_fail ;
    sep = merge_options c0.sep c1.sep ;

    var =  merge_substs c0.var c1.var ;
    paren =  merge_substs c0.paren c1.paren ;
    bracket =  merge_substs c0.bracket c1.bracket ;
    brace =  merge_substs c0.brace c1.brace ;
  }

let get_string table s =
  match EzToml.get_string table s with
  | exception Not_found ->
    Printf.kprintf failwith "missing field %S" (String.concat "." s)
  | v -> v

let register_special map key value =
  match key with
  | "env" ->
      Array.iter (fun kv ->
          let k,v = EzString.cut_at kv '=' in
          map := StringMap.add (value ^ k) v !map
        ) (Unix.environment ())
  | _ -> raise Not_found

let register_subst table loc =

  let inherits = EzToml.get_bool_default table [ "inherits" ] true in
  let active = EzToml.get_bool_default table [ "active" ] true in
  let map = match EzToml.get_table table [ "substs" ] with
    | exception Not_found -> StringMap.empty
    | table ->
        let map = ref StringMap.empty in
        EzToml.TYPES.Table.iter (fun key value ->
            let key = Toml.Types.Table.Key.to_string key in
            match value with
            | EzToml.TYPES.TString value ->
                Printf.eprintf "  %S -> %S\n%!" key value ;
                map := StringMap.add key (Some value) !map
            | EzToml.TYPES.TBool false ->
                map := StringMap.add key None !map
            | _ ->
                Printf.kprintf failwith
                  "Bad kind for %S %s"
                  key loc
          ) table;
        !map
  in
  {
    inherits ;
    active ;
    map ;
  }

let read_subst filename table section =
  match EzToml.get_table table [section] with
  | table ->
      Some (
        register_subst table
          (Printf.sprintf "in section %S of file %S" section filename)
      )
  | exception _ ->
      Printf.eprintf "  No section %S\n%!" section;
      None

let get_char_option filename table key =
  match EzToml.get_string table key with
  | exception Not_found -> None
  | s ->
      if String.length s = 1 then Some s.[0] else
        Printf.kprintf failwith "Wrong size for %s in file %s"
          ( String.concat "." key ) filename

let parse_config filename table =
  {
    escape = EzToml.get_bool_option table [ "config" ; "escape" ] ;
    sym = EzToml.get_bool_option table [ "config" ; "symetric" ] ;
    sym_fail = EzToml.get_bool_option table [ "config" ; "symetric-fails" ] ;
    sep = get_char_option filename table [ "config" ; "sep" ] ;

    var = read_subst filename table "var" ;
    paren = read_subst filename table "paren" ;
    brace = read_subst filename table "brace" ;
    bracket = read_subst filename table "bracket" ;
  }

let read_config filename =
  Printf.eprintf "Reading config %S\n%!" filename ;
  let table = EzToml.EZ.from_file_exn filename in
  parse_config filename table

let read_template filename =
  Printf.eprintf "Reading templace %S\n%!" filename ;
  let table = EzToml.EZ.from_file_exn filename in
  let src = EzToml.get_string table [ "template" ; "src" ] in
  let dst = Some (EzToml.get_string table [ "template" ; "dst" ]) in
  let config = parse_config filename table in
  let config =
    match EzToml.get_string_option table [ "template" ; "config" ] with
    | None -> config
    | Some filename ->
        let c0 = read_config filename in
        merge_configs c0 config
  in
  let loc = Printf.sprintf "file %S" filename in
  {
    src ; dst ; config ; loc
  }

let subst_of_subst kind s ~src =
  match s with
  | None -> None
  | Some s ->
      let fonction iter k =
        let f,arg = EzString.cut_at k ':' in
        match f with
        | "src" -> src
        | "env" -> begin
            try Sys.getenv arg with
            | Not_found ->
                Printf.kprintf failwith "No env: variable %S" arg
          end
        | "include" -> EzFile.read_file arg
        | "input" -> iter ( EzFile.read_file arg )
        | _ ->
            Printf.kprintf failwith "No occurrence for %s %S" kind k
      in
      let subst iter k =
        match StringMap.find k s.map with
        | exception Not_found -> fonction iter k
        | Some s -> s
        | None -> fonction iter k
      in
      Some subst

let perform_file_templating ?dst ~src t =
  Printf.eprintf "  File %s\n%!" src ;
  let content = EzFile.read_file src in
  let sep = match t.config.sep with
    | None -> '$'
    | Some sep -> sep
  in
  let escape = ref ( match t.config.escape with
      | None -> true
      | Some escape -> escape )
  in
  let sym = match t.config.sym with
    | None -> false
    | Some sym ->
        Printf.eprintf "symetric %b\n%!" sym;
        sym
  in
  let fail = match t.config.sym_fail with
    | None -> true
    | Some fail ->
        Printf.eprintf "symetric fails %b\n%!" fail;
        fail
  in
  let var = subst_of_subst "var" t.config.var ~src in
  let paren = subst_of_subst "paren" t.config.paren ~src in
  let brace = subst_of_subst "brace" t.config.brace ~src in
  let bracket = subst_of_subst "bracket" t.config.bracket ~src in
  let rec iter content =
    EZ_SUBST.string
      ~sep
      ~ctxt:iter
      ~escape
      ~sym
      ~fail
      ~skipper:(ref [])
      ?var
      ?brace
      ?paren
      ?bracket
      content
  in
  let content = try iter content with
    | EZ_SUBST.UnclosedExpression s  ->
        Printf.kprintf failwith "Unclosed/asymetric expression %S" s
  in
  match dst with
  | None ->
      print_string content
  | Some dst ->
      Printf.eprintf "    Generating %s\n%!" dst ;
      EzFile.write_file dst content

let perform_template t =
  let src = t.src in
  let dst = (* match t.config.root with
               | None -> *) t.dst
  (*    | Some root -> root // t.dst *)
  in
  Printf.eprintf "Template %s\n%!" t.loc;
  match dst with
  | None ->
      perform_file_templating t ~src
  | Some dst ->
      let rec iter ~src ~dst =
        let st = Unix.lstat src in
        match st.Unix.st_kind with
        | S_REG -> perform_file_templating t ~src ~dst
        | S_DIR ->
            let files = Sys.readdir src in
            EzFile.safe_mkdir dst;
            Array.iter (fun file ->
                let src = src // file in
                let dst = dst // file in
                iter ~src ~dst
              ) files
        | S_LNK ->
            let link = Unix.readlink src in
            Unix.link link dst
        | _ ->
            Printf.kprintf failwith "Unsupprted type for file %S\n%!" src
      in
      iter ~src ~dst

let set_var config subst = config.var <- subst
let set_paren config subst = config.paren <- subst
let set_brace config subst = config.brace <- subst
let set_bracket config subst = config.bracket <- subst

let get_var config = config.var
let get_paren config = config.paren
let get_brace config = config.brace
let get_bracket config = config.bracket

let with_subst subst =
  match subst with
  | None -> new_subst ()
  | Some subst -> subst

let add_subst kv subst =
  let k,v = EzString.cut_at kv '=' in
  subst.map <- StringMap.add k (Some v) subst.map;
  Some subst

let set_active bool subst =
  subst.active <- bool ;
  Some subst

let set_inherits bool subst =
  subst.inherits <- bool ;
  Some subst

let main () =
  (* We use 2 configs, the first one for inherits and reading config files,
     the second one for command line arguments. Both are merged at the end. *)
  let config1 = ref @@ new_config ()  in
  let config2 = new_config () in
  let templates = ref [] in
  let input = ref None in
  let output = ref None in
  let arg_list =
    [

      "-c", Arg.String (fun s ->
          let c = read_config s in
          config1 := merge_configs !config1 c),
      "FILE Read this configuration file";

      "-t", Arg.String (fun s -> templates := s :: !templates),
      "FILE Read this template file";

      "-o", Arg.String (fun s -> output := Some s),
      "FILE Output to file";

      "--sep", Arg.String (fun sep ->
          if String.length sep <> 1 then
            Printf.kprintf failwith "Wrong size for separator %S" sep;
          config2.sep <- Some sep.[0]),
      "SEP Set separator ($ is the default)";

      "--escape", Arg.Bool (fun bool ->
          config2.escape <- Some bool),
      "BOOL Can escaping be performed with backslashes";

      "--symetric", Arg.Bool (fun bool ->
          config2.sym <- Some bool),
      "BOOL Should variables use a symetric notation (${x}$)";

    ]
  in
  let arg_list =
    arg_list @
    List.flatten @@
    List.map (fun (kind, getter, setter) ->
        [

          Printf.sprintf "--%s" kind,
          Arg.String (fun s ->
              setter config2 @@ add_subst s @@ with_subst @@ getter config2 ;
            ),
          Printf.sprintf "k=v Add this %s substitution" kind;

          Printf.sprintf "--%s.active" kind,
          Arg.Bool (fun bool ->
              setter config2 @@ set_active bool @@ with_subst @@ getter config2
            ),
          Printf.sprintf "BOOL Whether the %s substitution is active" kind ;

          (* NOTE: inherits is performed on config1 instead of config2 *)
          Printf.sprintf "--%s.inherits" kind,
          Arg.Bool (fun bool ->
              setter !config1 @@ set_inherits bool @@
              with_subst @@ getter !config1
            ),
          Printf.sprintf "BOOL Whether the %s substitution is active" kind ;

        ]
      ) [
      "var", get_var, set_var ;
      "paren", get_paren, set_paren ;
      "brace", get_brace, set_brace ;
      "bracket", get_bracket, set_bracket ;
    ]
  in
  let arg_list = Arg.align arg_list in

  Arg.parse arg_list
    (fun s ->
       match !input with
       | None -> input := Some s
       | Some _ ->
           Printf.kprintf failwith
             "Unexpected second anonymous argument %S" s)
    "ocp-subst [OPTIONS] [INPUT] Perform substitutions" ;

  let config = merge_configs !config1 config2 in
  let templates = List.map read_template !templates in
  let templates = List.map (fun t ->
      let config = merge_configs t.config config in
      { t with config }
    ) templates
  in
  match !input with
  | None ->
      List.iter perform_template templates
  | Some src ->
      let t = {
        loc = "Arguments";
        src ;
        dst = !output ;
        config = config ;
      } in
      List.iter perform_template ( t :: templates )

let () =
  try
    main ()
  with
  | Failure msg ->
      Printf.eprintf "Fatal error: %s\n%!" msg ;
      exit 2
