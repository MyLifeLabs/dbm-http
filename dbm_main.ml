(*
  HTTP wrapper around a DBM database.
  Keys = strings,
  Values = JSON or binary
*)

(*
  Command-line modes:

  - dbm load DB CREATE|UPDATE JSON?
  - dbm dump DB JSON?
  - dbm get DB JSON? KEY
  - dbm set DB JSON? KEY [VALUE]
  - dbm http DB RW? PORT=? LOG=? CONTENT-TYPE=?

  HTTP modes:

  - get
  - set (if enabled)
  - info
*)

open Printf

let anon_args = ref []
let anon_fun s =
  anon_args := s :: !anon_args

let get_anon_args () =
  List.rev !anon_args

let enter_load_mode () =
  let data_format = ref (`Json : Dbm_base.data_format) in
  let db_mode = ref (`Create : Dbm_base.db_mode) in
  let options = [
    "-create",
    Arg.Unit (fun () -> db_mode := `Create),
    "
          [default] Create new database or fail if it already exists.";

    "-update",
    Arg.Unit (fun () -> db_mode := `Update),
    "
          Add key/value bindings to an existing database, or fail
          if the database does not exist. Existing bindings are updated.";

    "-json",
    Arg.Unit (fun () -> data_format := `Json),
    "
          [default] Data are provided as whitespace-separated
          UTF-8 JSON records of the form { \"key\": KEY, \"value\": VALUE }
          where KEY is a JSON string and VALUE can be any valid JSON value.
          KEY and VALUE represent the key and value to be stored
          in the database.";

    "-hex",
    Arg.Unit (fun () -> data_format := `Hex),
    "
          Data are provided as whitespace-separated
          UTF-8 JSON records of the form { \"key\": KEY, \"value\": VALUE }
          where KEY is a JSON string representing the key to be stored
          into the database and VALUE is a JSON string containing
          the hexadecimal representation of the value.";
  ]
  in
  let usage_msg = "\
    Usage: dbm load <database file name> [options]
"
  in
  let on_error () =
    Arg.usage options usage_msg;
    exit 1
  in
  (try
     Arg.parse_argv Sys.argv options anon_fun usage_msg
   with e ->
     on_error ()
  );
  match get_anon_args () with
      [ db ] -> Dbm_base.load db !db_mode !data_format stdin
    | _ -> on_error ()


let enter_dump_mode () = failwith "not implemented 1"
let enter_get_mode () = failwith "not implemented 2"
let enter_set_mode () = failwith "not implemented 3"
let enter_http_mode () = failwith "not implemented 4"

let general_usage oc =
  fprintf oc "\
Usage:
  dbm load DBFILE [OPTIONS]     # put key/value JSON records into database
  dbm dump DBFILE [OPTIONS]     # extract key/value JSON records from database
  dbm get DBFILE KEY [OPTIONS]           # get specific record from database
  dbm set DBFILE KEY [VALUE] [OPTIONS]   # set/replace value
  dbm http DBFILE [OPTIONS]              # start HTTP frontend
  dbm help [OPTIONS]            # display this help message

Each mode has a -help option, e.g. \"dbm load -help\" 
documents the \"load\" mode.
"

let main () =
  if Array.length Sys.argv < 2 then (
    general_usage stderr;
    exit 1
  )
  else (
    let mode = Sys.argv.(1) in
    incr Arg.current;
    match mode with
        "load" -> enter_load_mode ()
      | "dump" -> enter_dump_mode ()
      | "get" -> enter_get_mode ()
      | "set" -> enter_set_mode ()
      | "http" -> enter_http_mode ()
      | "help" | "-help" | "--help" ->
          general_usage stdout;
          exit 0
      | mode ->
          eprintf "Invalid mode %S\n" mode;
          general_usage stderr;
          exit 1
  )

let () = main ()
