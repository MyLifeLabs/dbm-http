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

    "-raw",
    Arg.Unit (fun () -> data_format := `Raw),
    "
          Data are provided as whitespace-separated
          UTF-8 JSON records of the form { \"key\": KEY, \"value\": VALUE }
          where KEY is a JSON string representing the key to be stored
          into the database and VALUE is a JSON string representing
          the value.";
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

let enter_get_mode () =
  let data_format = ref (`Json : Dbm_base.data_format) in
  let options = [
    "-json",
    Arg.Unit (fun () -> data_format := `Json),
    "
          [default] Indicates that the values of the database are
          JSON-compliant, allowing them to be pretty-printed.";

    "-hex",
    Arg.Unit (fun () -> data_format := `Hex),
    "
          Indicates that the values of the database must be
          returned hex-encoded.";

    "-raw",
    Arg.Unit (fun () -> data_format := `Raw),
    "
          Indicates that the values of the database must be
          returned as-is.";
  ]
  in
  let usage_msg = "\
    Usage: dbm get <database file name> <key> [options]
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
      [ db; key ] ->
        (match Dbm_base.get_formatted !data_format db key with
             None ->
               exit 1
           | Some s ->
               print_string s;
               exit 0)
    | _ -> on_error ()


let enter_set_mode () =
  let data_format = ref (`Json : Dbm_base.data_format) in
  let options = [
    "-json",
    Arg.Unit (fun () -> data_format := `Json),
    "
          [default] Indicates that the values of the database are
          JSON-compliant, allowing them to be pretty-printed.";

    "-hex",
    Arg.Unit (fun () -> data_format := `Hex),
    "
          Indicates that the values of the database must be
          returned hex-encoded.";

    "-raw",
    Arg.Unit (fun () -> data_format := `Raw),
    "
          Indicates that the values of the database must be
          returned as-is.";
  ]
  in
  let usage_msg = "\
    Usage: dbm set <database file name> <key> <value> [options]
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
      [ db; key; value ] ->
        Dbm_base.set_formatted !data_format db key value
    | _ -> on_error ()


let enter_http_mode () =
  let content_type = ref "text/plain" in
  let data_format = ref (`Json : Dbm_base.data_format) in
  let logfile = ref "dbm-http.log" in
  let port = ref 80 in
  let rw = ref false in
  let options = [
    "-content-type",
    Arg.Set_string content_type,
    "<TYPE/SUBTYPE>
          Specifies the value of the HTTP response's Content-type field
          for successful lookup operations.
          Default: text/plain";

    "-json",
    Arg.Unit (fun () -> data_format := `Json),
    "
          [default] Indicates that the values of the database are
          JSON-compliant, allowing them to be pretty-printed.";

    "-hex",
    Arg.Unit (fun () -> data_format := `Hex),
    "
          Indicates that the values of the database must be
          returned hex-encoded.";

    "-raw",
    Arg.Unit (fun () -> data_format := `Raw),
    "
          Indicates that the values of the database must be
          returned as-is.";

    "-port",
    Arg.Set_int port,
    "<PORT NUMBER>
          Specifies the port on which the HTTP server listens.
          Default: 80";

    "-log",
    Arg.Set_string logfile,
    "<FILENAME>
          Specifies the log file.
          Default: dbm-http.log";

    "-rw",
    Arg.Set rw,
    "
          Enables write access via /set.";
  ]
  in
  let usage_msg = "\
    Usage: dbm http <database file name> [options]
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
      [ db ] ->
        let p = {
          Dbm_http.db_name = db;
          rw = !rw;
          content_type = !content_type;
          port = !port;
          logfile = !logfile;
          format = !data_format;
        }
        in
        Dbm_http.start p
    | _ -> on_error ()


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
  Printexc.record_backtrace true;
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
